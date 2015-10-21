{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module GDAL.Internal.Warper (
    ResampleAlg (..)
  , WarpOptions (..)
  , BandOptions (..)
  , GDALWarpException (..)
  , reprojectImage
  , setTransformer
  , createWarpedVRT
  , def
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, void, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception(..), bracket)
import Data.Maybe (isJust, fromMaybe)
import Data.Typeable (Typeable)
import Data.Default (Default(..))
import Data.Proxy (Proxy)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..), CChar(..))
import Foreign.Ptr (
    Ptr
  , FunPtr
  , nullPtr
  , castPtr
  )
import Foreign.Marshal.Utils (with)
import Foreign.Storable (Storable(..))

import GDAL.Internal.Types
import GDAL.Internal.Util (fromEnumC)
import GDAL.Internal.CPLConv
{#import GDAL.Internal.Algorithms #}
{#import GDAL.Internal.CPLError #}
{#import GDAL.Internal.CPLString #}
{#import GDAL.Internal.CPLProgress #}
{#import GDAL.Internal.OSR #}
{#import GDAL.Internal.GDAL #}

#include "gdal.h"
#include "gdalwarper.h"

data GDALWarpException
  = WarpStopped
  deriving (Typeable, Show, Eq)

instance NFData GDALWarpException where
  rnf a = a `seq` ()

instance Exception GDALWarpException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

{# enum GDALResampleAlg as ResampleAlg {upcaseFirstLetter} with prefix = "GRA_"
     deriving (Eq,Read,Show,Bounded) #}

data BandOptions = forall a b. (GDALType a, GDALType b)
  => BandOptions {
       biSrc       :: !Int
     , biDst       :: !Int
     , biSrcNoData :: !(Maybe a)
     , biDstNoData :: !(Maybe b)
     }

deriving instance Show BandOptions


data WarpOptions s = forall t. Transformer t
  => WarpOptions {
      woResampleAlg     :: ResampleAlg
    , woWarpOptions     :: OptionList
    , woMemoryLimit     :: Double
    , woWorkingDatatype :: Datatype
    , woBands           :: [BandOptions]
    , woTransfomer      :: Maybe (t s)
    }

instance Default (WarpOptions s) where
  def = WarpOptions {
          woResampleAlg     = NearestNeighbour
        , woWarpOptions     = []
        , woMemoryLimit     = 0
        , woWorkingDatatype = GDT_Unknown
        , woBands           = []
        , woTransfomer      = Nothing :: Maybe (GenImgProjTransformer s)
        }

-- Avoids "Record update for insufficiently polymorphic field" when doigs
-- opts { woTransfomer = Just ...}
setTransformer
  :: forall t s. Transformer t
  => t s -> WarpOptions s -> WarpOptions s
setTransformer t opts = WarpOptions {
    woResampleAlg     = woResampleAlg opts
  , woWarpOptions     = woWarpOptions opts
  , woMemoryLimit     = woMemoryLimit opts
  , woWorkingDatatype = woWorkingDatatype opts
  , woBands           = woBands opts
  , woTransfomer      = Just t
  }



setOptionDefaults
  :: forall s t. RODataset s -> Maybe (Dataset s t) -> WarpOptions s
  -> GDAL s (WarpOptions s)
setOptionDefaults ds moDs wo@WarpOptions{..} = do
  bands <- if null woBands
            then do
              nBands <- datasetBandCount ds
              forM [1..nBands] $ \i -> do
                b <- getBand i ds
                reifyBandDatatype b $ \(_ :: Proxy a) -> do
                  srcNd <- bandNodataValue b :: GDAL s (Maybe a)
                  case moDs of
                    Just oDs -> do
                      b' <- getBand i oDs
                      reifyBandDatatype b' $ \(_ :: Proxy a') -> do
                        dstNd <- bandNodataValue b' :: GDAL s (Maybe a')
                        return (BandOptions i i srcNd dstNd)
                    Nothing  -> return (BandOptions i i srcNd srcNd)
            else return woBands
  let warpOptions
        | anyBandHasDstNoData wo' = ("INIT_DEST","NO_DATA") : woWarpOptions
        | otherwise               = woWarpOptions
      wo' = wo {woBands = bands}
  return wo' {woWarpOptions = warpOptions}

anyBandHasNoData :: WarpOptions s -> Bool
anyBandHasNoData wo
  = any (\BandOptions{..} -> isJust biSrcNoData || isJust biDstNoData) (woBands wo)

anyBandHasDstNoData :: WarpOptions s -> Bool
anyBandHasDstNoData wo = any (\BandOptions{..} -> isJust biDstNoData) (woBands wo)

withWarpOptionsPtr
  :: RODataset s -> WarpOptions s -> (Ptr (WarpOptions s) -> IO c) -> IO c
withWarpOptionsPtr ds wo@WarpOptions{..}
  = bracket createWarpOptions destroyWarpOptions
  where
    dsPtr = unDataset ds
    createWarpOptions = do
      p <- c_createWarpOptions
      {#set GDALWarpOptions.hSrcDS #} p (castPtr dsPtr)
      {#set GDALWarpOptions.eResampleAlg #} p (fromEnumC woResampleAlg)
      oListPtr <- toOptionListPtr woWarpOptions
      {#set GDALWarpOptions.papszWarpOptions #} p oListPtr
      {#set GDALWarpOptions.dfWarpMemoryLimit #} p (realToFrac woMemoryLimit)
      {#set GDALWarpOptions.eWorkingDataType #} p (fromEnumC woWorkingDatatype)
      {#set GDALWarpOptions.nBandCount #} p (fromIntegral (length woBands))
      {#set GDALWarpOptions.panSrcBands #} p =<<
        listToArray (map (fromIntegral . biSrc) woBands)
      {#set GDALWarpOptions.panDstBands #} p =<<
        listToArray (map (fromIntegral . biDst) woBands)
      -- ignores finalizer since destroyWarpOptions takes care of it
      (t, tArg, _) <- createTransformerAndArg woTransfomer
      {#set GDALWarpOptions.pfnTransformer #} p (getTransformerFunPtr t)
      {#set GDALWarpOptions.pTransformerArg #} p (castPtr tArg)
      when (anyBandHasNoData wo) $ do
        vPtrSrcR <- listToArray
                      (map (\BandOptions{..} ->
                              toCDouble (fromMaybe nodata biSrcNoData)) woBands)
        vPtrSrcI <- listToArray (replicate (length woBands) 0)
        vPtrDstR <- listToArray
                      (map (\BandOptions{..} ->
                              toCDouble (fromMaybe nodata biDstNoData)) woBands)
        vPtrDstI <- listToArray (replicate (length woBands) 0)
        {#set GDALWarpOptions.padfDstNoDataReal #} p vPtrDstR
        {#set GDALWarpOptions.padfDstNoDataImag #} p vPtrDstI
        {#set GDALWarpOptions.padfSrcNoDataReal #} p vPtrSrcR
        {#set GDALWarpOptions.padfSrcNoDataImag #} p vPtrSrcI
      return p

    destroyWarpOptions = c_destroyWarpOptions

foreign import ccall unsafe "gdalwarper.h GDALCreateWarpOptions"
  c_createWarpOptions :: IO (Ptr (WarpOptions s))

foreign import ccall unsafe "gdalwarper.h GDALDestroyWarpOptions"
  c_destroyWarpOptions :: Ptr (WarpOptions s) -> IO ()

reprojectImage
  :: RODataset s
  -> Maybe SpatialReference
  -> RWDataset s
  -> Maybe SpatialReference
  -> ResampleAlg
  -> Double
  -> Double
  -> Maybe ProgressFun
  -> OptionList
  -> GDAL s ()
reprojectImage srcDs srcSrs dstDs dstSrs algo memLimit maxError progressFun opts
  = do options' <- setOptionDefaults srcDs (Just dstDs)
                     (def {woWarpOptions=opts})
       ret <- liftIO $
          withProgressFun progressFun $ \pFun ->
          throwIfError "reprojectImage" $
          withLockedDatasetPtr srcDs $ \srcPtr ->
          withLockedDatasetPtr dstDs $ \dstPtr ->
          withMaybeSRAsCString srcSrs $ \srcSrs' ->
          withMaybeSRAsCString dstSrs $ \dstSrs' ->
          withWarpOptionsPtr srcDs options' $ \wopts ->
            void $ c_reprojectImage srcPtr srcSrs' dstPtr dstSrs' algo'
                     memLimit' maxError' pFun nullPtr wopts
       maybe (throwBindingException WarpStopped) return ret
  where
    algo'     = fromEnumC algo
    maxError' = realToFrac maxError
    memLimit' = realToFrac memLimit

foreign import ccall safe "gdalwarper.h GDALReprojectImage" c_reprojectImage
  :: Ptr (Dataset s t)   -- ^Source dataset
  -> CString             -- ^Source proj (WKT)
  -> Ptr (RWDataset s)   -- ^Dest dataset
  -> CString             -- ^Dest proj (WKT)
  -> CInt                -- ^Resample alg
  -> CDouble             -- ^Memory limit
  -> CDouble             -- ^Max error
  -> ProgressFunPtr      -- ^Progress func
  -> Ptr ()              -- ^Progress arg (unused)
  -> Ptr (WarpOptions s) -- ^warp options
  -> IO CInt

createWarpedVRT
  :: forall s. RODataset s
  -> Size
  -> Geotransform
  -> WarpOptions s
  -> GDAL s (RODataset s)
createWarpedVRT srcDs (XY nPixels nLines) geotransform wo@WarpOptions{..} = do
  options'' <- setOptionDefaults srcDs Nothing options'
  newDsPtr <- liftIO $
    withWarpOptionsPtr srcDs options'' $ \opts ->
    with geotransform $ \gt -> do
      pArg <- {#get GDALWarpOptions.pTransformerArg #} opts
      when (pArg /= nullPtr) $
        {#call GDALSetGenImgProjTransformerDstGeoTransform as ^#} pArg
          (castPtr gt)
      c_createWarpedVRT dsPtr nPixels' nLines' (castPtr gt) opts
  oDs <- newDerivedDatasetHandle srcDs newDsPtr
  setDstNodata oDs options''
  unsafeToReadOnly oDs
  where
    nPixels' = fromIntegral nPixels
    nLines'  = fromIntegral nLines
    dsPtr    = unDataset srcDs
    options' = case woTransfomer of
                 Nothing -> setTransformer (def :: GenImgProjTransformer s) wo
                 Just _  -> wo

setDstNodata :: RWDataset s -> WarpOptions s -> GDAL s ()
setDstNodata oDs options
  = when (anyBandHasDstNoData options) $
      forM_ (woBands options) $ \BandOptions{..} ->
        case biDstNoData of
          Just nd -> do
            b <- getBand biDst oDs
            setBandNodataValue b nd
          Nothing -> return ()

foreign import ccall safe "gdalwarper.h GDALCreateWarpedVRT" c_createWarpedVRT
  :: Ptr (RODataset s)    -- ^Source dataset
  -> CInt                 -- ^nPixels
  -> CInt                 -- ^nLines
  -> Ptr CDouble          -- ^geotransform
  -> Ptr (WarpOptions s)  -- ^warp options
  -> IO (Ptr (RWDataset s))
