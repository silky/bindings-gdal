{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

#include "bindings.h"

module GDAL.Internal.GDAL (
    Geotransform (..)
  , OverviewResampling (..)
  , MaskType (..)

  , northUpGeotransform
  , gcpGeotransform
  , applyGeotransform
  , (|$|)
  , invertGeotransform
  , inv
  , composeGeotransforms
  , (|.|)

  , allRegister
  , destroyDriverManager
  , create
  , createMem
  , delete
  , rename
  , copyFiles
  , flushCache
  , closeDataset
  , openReadOnly
  , openReadWrite
  , unsafeToReadOnly
  , createCopy
  , buildOverviews
  , driverCreationOptionList

  , datasetDriver
  , datasetSize
  , datasetFileList
  , datasetProjection
  , setDatasetProjection
  , datasetGeotransform
  , setDatasetGeotransform
  , datasetGCPs
  , setDatasetGCPs

  , metadataDomains
  , metadata
  , metadataItem
  , setMetadataItem
  , description
  , setDescription

  , version
  , newDatasetHandle
  , openDatasetCount
) where

{#context lib = "gdal" prefix = "GDAL" #}


{#import GDAL.Internal.GDAL.Types#}
import GDAL.Internal.DataType
import GDAL.Internal.Types
import GDAL.Internal.Common
import GDAL.Internal.Util
{#import GDAL.Internal.GCP#}
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.CPLProgress#}
import GDAL.Internal.OSR

import Control.Applicative ((<$>), (<*>), liftA2, pure)
import Control.Exception (Exception(..))
import Control.Monad (liftM, liftM2, when, (>=>), void)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Bits ((.&.))
import Data.Int
import Data.Complex
import Data.Word
import Data.ByteString.Char8 (ByteString, packCString, useAsCString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Stm
import qualified Data.Vector.Generic.Mutable as GM
import Data.Word (Word8)

import Foreign.C.String (withCString, CString)
import Foreign.C.Types
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (toBool, fromBool, with)

import GHC.Types (SPEC(..))

import System.IO.Unsafe (unsafePerformIO)


#include "gdal.h"

version :: (Int, Int)
version = ({#const GDAL_VERSION_MAJOR#} , {#const GDAL_VERSION_MINOR#})



{#fun AllRegister as ^ {} -> `()'  #}

{#fun DestroyDriverManager as ^ {} -> `()'#}

driverByName :: Driver -> IO DriverH
driverByName (Driver s) = do
  d <- useAsCString s {#call unsafe GetDriverByName as ^#}
  if d == nullPtr
    then throwBindingException (UnknownDriver s)
    else return d

driverCreationOptionList :: Driver -> ByteString
driverCreationOptionList driver = unsafePerformIO $ do
  d <- driverByName driver
  {#call GetDriverCreationOptionList	as ^#} d >>= packCString

validateCreationOptions :: DriverH -> Ptr CString -> IO ()
validateCreationOptions d o = do
  valid <- liftM toBool ({#call GDALValidateCreationOptions as ^ #} d o)
  when (not valid) (throwBindingException InvalidDriverOptions)

create
  :: Driver -> String -> Size -> Int -> DataType -> OptionList
  -> GDAL s (Dataset s ReadWrite)
create drv path (XY nx ny) bands dtype  options
  | dtype == gdtUnknown = throwBindingException UnknownRasterDataType
  | otherwise
  = newDatasetHandle $ withCString path $ \path' -> do
      let nx'    = fromIntegral nx
          ny'    = fromIntegral ny
          bands' = fromIntegral bands
          dtype' = fromEnumC dtype
      d <- driverByName drv
      withOptionList options $ \opts -> do
        validateCreationOptions d opts
        {#call GDALCreate as ^#} d path' nx' ny' bands' dtype' opts

closeDataset :: MonadIO m => Dataset s t -> m ()
closeDataset (Dataset (rk,_)) = release rk

delete :: Driver -> String -> GDAL s ()
delete driver path =
  liftIO $
  checkCPLError "deleteDataset" $ do
    d <- driverByName driver
    withCString path ({#call DeleteDataset as ^#} d)

rename :: Driver -> String -> String -> GDAL s ()
rename driver newName oldName =
  liftIO $
  checkCPLError "renameDataset" $ do
    d <- driverByName driver
    withCString newName $
      withCString oldName . ({#call RenameDataset as ^#} d)

copyFiles :: Driver -> String -> String -> GDAL s ()
copyFiles driver newName oldName =
  liftIO $
  checkCPLError "copyFiles" $ do
    d <- driverByName driver
    withCString newName $
      withCString oldName . ({#call CopyDatasetFiles as ^#} d)

openReadOnly :: String -> GDAL s (RODataset s)
openReadOnly p = openWithMode GA_ReadOnly p

openReadWrite :: String -> GDAL s (RWDataset s)
openReadWrite p = openWithMode GA_Update p

openWithMode :: GDALAccess -> String -> GDAL s (Dataset s t)
openWithMode m path =
  newDatasetHandle $
  withCString path $
  flip {#call GDALOpen as ^#} (fromEnumC m)

unsafeToReadOnly :: RWDataset s -> GDAL s (RODataset s)
unsafeToReadOnly ds = flushCache ds >> return (coerce ds)

createCopy
  :: Driver -> String -> Dataset s t -> Bool -> OptionList
  -> Maybe ProgressFun -> GDAL s (RWDataset s)
createCopy driver path ds strict options progressFun =
  newDatasetHandle $
  withProgressFun "createCopy" progressFun $ \pFunc -> do
    d <- driverByName driver
    withOptionList options $ \o -> do
      validateCreationOptions d o
      withCString path $ \p ->
        {#call GDALCreateCopy as ^#}
          d p (unDataset ds) (fromBool strict) o pFunc nullPtr



newDatasetHandle :: IO DatasetH -> GDAL s (Dataset s t)
newDatasetHandle act =
  liftM Dataset $ allocate (checkGDALCall checkit act) free
  where
    checkit exc p
      | p==nullDatasetH = Just (fromMaybe
                                (GDALBindingException NullDataset) exc)
      | otherwise       = Nothing
    free ds = do
      refCount <- {#call unsafe DereferenceDataset as ^#} ds
      when (refCount<=0) ({#call GDALClose as ^#} ds)

createMem
  :: Size -> Int -> DataType -> OptionList -> GDAL s (Dataset s ReadWrite)
createMem = create "Mem" ""

flushCache :: forall s. RWDataset s -> GDAL s ()
flushCache = liftIO . {#call GDALFlushCache as ^#} . unDataset

datasetDriver :: Dataset s t -> Driver
datasetDriver ds =
  unsafePerformIO $
  liftM Driver $ do
    driver <-{#call unsafe GetDatasetDriver as ^#} (unDataset ds)
    {#call unsafe GetDriverShortName as ^#} driver >>= packCString

datasetSize :: Dataset s t -> Size
datasetSize ds =
  fmap fromIntegral $
     XY ({#call pure unsafe GetRasterXSize as ^#} (unDataset ds))
        ({#call pure unsafe GetRasterYSize as ^#} (unDataset ds))

datasetFileList :: Dataset s t -> GDAL s [Text]
datasetFileList =
  liftIO .
  fromCPLStringList .
  {#call unsafe GetFileList as ^#} .
  unDataset

datasetProjection :: Dataset s t -> GDAL s (Maybe SpatialReference)
datasetProjection =
  liftIO . ({#call unsafe GetProjectionRef as ^#} . unDataset
              >=> maybeSpatialReferenceFromCString)


setDatasetProjection :: RWDataset s -> SpatialReference -> GDAL s ()
setDatasetProjection ds srs =
  liftIO $ checkCPLError "SetProjection" $
    unsafeUseAsCString (srsToWkt srs)
      ({#call unsafe SetProjection as ^#} (unDataset ds))

setDatasetGCPs
  :: RWDataset s -> [GroundControlPoint] -> Maybe SpatialReference -> GDAL s ()
setDatasetGCPs ds gcps mSrs =
  liftIO $
  checkCPLError "setDatasetGCPs" $
  withGCPArrayLen gcps $ \nGcps pGcps ->
  withMaybeSRAsCString mSrs $
  {#call unsafe GDALSetGCPs as ^#}
    (unDataset ds)
    (fromIntegral nGcps)
    pGcps

datasetGCPs
  :: Dataset s t
  -> GDAL s ([GroundControlPoint], Maybe SpatialReference)
datasetGCPs ds =
  liftIO $ do
    let pDs = unDataset ds
    srs <- maybeSpatialReferenceFromCString
            =<< {#call unsafe GetGCPProjection as ^#} pDs
    nGcps <- liftM fromIntegral ({#call unsafe GetGCPCount as ^#} pDs)
    gcps <- {#call unsafe GetGCPs as ^#} pDs >>= fromGCPArray nGcps
    return (gcps, srs)


data OverviewResampling
  = OvNearest
  | OvGauss
  | OvCubic
  | OvAverage
  | OvMode
  | OvAverageMagphase
  | OvNone

buildOverviews
  :: RWDataset s -> OverviewResampling -> [Int] -> [Int] -> Maybe ProgressFun
  -> GDAL s ()
buildOverviews ds resampling overviews bands progressFun =
  liftIO $
  withResampling resampling $ \pResampling ->
  withArrayLen (map fromIntegral overviews) $ \nOverviews pOverviews ->
  withArrayLen (map fromIntegral bands) $ \nBands pBands ->
  withProgressFun "buildOverviews" progressFun $ \pFunc ->
  checkCPLError "buildOverviews" $
    {#call GDALBuildOverviews as ^#}
      (unDataset ds)
      pResampling
      (fromIntegral nOverviews)
      pOverviews
      (fromIntegral nBands)
      pBands
      pFunc
      nullPtr
  where
    withResampling OvNearest         = unsafeUseAsCString "NEAREST\0"
    withResampling OvGauss           = unsafeUseAsCString "GAUSS\0"
    withResampling OvCubic           = unsafeUseAsCString "CUBIC\0"
    withResampling OvAverage         = unsafeUseAsCString "AVERAGE\0"
    withResampling OvMode            = unsafeUseAsCString "MODE\0"
    withResampling OvAverageMagphase = unsafeUseAsCString "AVERAGE_MAGPHASE\0"
    withResampling OvNone            = unsafeUseAsCString "NONE\0"

data Geotransform
  = Geotransform {
      gtXOff   :: {-# UNPACK #-} !Double
    , gtXDelta :: {-# UNPACK #-} !Double
    , gtXRot   :: {-# UNPACK #-} !Double
    , gtYOff   :: {-# UNPACK #-} !Double
    , gtYRot   :: {-# UNPACK #-} !Double
    , gtYDelta :: {-# UNPACK #-} !Double
  } deriving (Eq, Show)

northUpGeotransform :: Size -> Envelope Double -> Geotransform
northUpGeotransform size envelope =
  Geotransform {
      gtXOff   = px (envelopeMin envelope)
    , gtXDelta = px (envelopeSize envelope / fmap fromIntegral size)
    , gtXRot   = 0
    , gtYOff   = py (envelopeMax envelope)
    , gtYRot   = 0
    , gtYDelta = negate (py (envelopeSize envelope / fmap fromIntegral size))
  }

gcpGeotransform :: [GroundControlPoint] -> ApproxOK -> Maybe Geotransform
gcpGeotransform gcps approxOk =
  unsafePerformIO $
  alloca $ \pGt ->
  withGCPArrayLen gcps $ \nGcps pGcps -> do
    ret <- liftM toBool $
           {#call unsafe GCPsToGeoTransform as ^#}
             (fromIntegral nGcps)
             pGcps
             (castPtr pGt)
             (fromEnumC approxOk)
    if ret then liftM Just (peek pGt) else return Nothing

applyGeotransform :: Geotransform -> XY Double -> XY Double
applyGeotransform Geotransform{..} (XY x y) =
  XY (gtXOff + gtXDelta*x + gtXRot   * y)
     (gtYOff + gtYRot  *x + gtYDelta * y)
{-# INLINE applyGeotransform #-}

infixr 5 |$|
(|$|) :: Geotransform -> XY Double -> XY Double
(|$|) = applyGeotransform

invertGeotransform :: Geotransform -> Maybe Geotransform
invertGeotransform (Geotransform g0 g1 g2 g3 g4 g5)
  | g2==0 && g4==0 && g1/=0 && g5/=0 -- No rotation
  = Just $! Geotransform (-g0/g1) (1/g1) 0 (-g3/g5) 0 (1/g5)
  | abs det < 1e-15 = Nothing -- not invertible
  | otherwise
  = Just $! Geotransform ((g2*g3 - g0*g5) * idet)
                         (g5*idet)
                         (-g2*idet)
                         ((-g1*g3 + g0*g4) * idet)
                         (-g4*idet)
                         (g1*idet)
  where
    idet = 1/det
    det  = g1*g5 - g2*g4
{-# INLINE invertGeotransform #-}

inv :: Geotransform -> Geotransform
inv = fromMaybe (error "Could not invert geotransform") . invertGeotransform

-- | Creates a Geotransform equivalent to applying a and then b
composeGeotransforms :: Geotransform -> Geotransform -> Geotransform
b `composeGeotransforms` a =
  Geotransform (b1*a0 + b2*a3 + b0)
               (b1*a1 + b2*a4)
               (b1*a2 + b2*a5)
               (b4*a0 + b5*a3 + b3)
               (b4*a1 + b5*a4)
               (b4*a2 + b5*a5)

  where
    Geotransform a0 a1 a2 a3 a4 a5 = a
    Geotransform b0 b1 b2 b3 b4 b5 = b
{-# INLINE composeGeotransforms #-}

infixr 9 |.|
(|.|) :: Geotransform -> Geotransform -> Geotransform
(|.|) = composeGeotransforms

instance Storable Geotransform where
  sizeOf _     = sizeOf (undefined :: CDouble) * 6
  alignment _  = alignment (undefined :: CDouble)
  poke pGt (Geotransform g0 g1 g2 g3 g4 g5) = do
    let p = castPtr pGt :: Ptr CDouble
    pokeElemOff p 0 (realToFrac g0)
    pokeElemOff p 1 (realToFrac g1)
    pokeElemOff p 2 (realToFrac g2)
    pokeElemOff p 3 (realToFrac g3)
    pokeElemOff p 4 (realToFrac g4)
    pokeElemOff p 5 (realToFrac g5)
  peek pGt = do
    let p = castPtr pGt :: Ptr CDouble
    Geotransform <$> liftM realToFrac (peekElemOff p 0)
                 <*> liftM realToFrac (peekElemOff p 1)
                 <*> liftM realToFrac (peekElemOff p 2)
                 <*> liftM realToFrac (peekElemOff p 3)
                 <*> liftM realToFrac (peekElemOff p 4)
                 <*> liftM realToFrac (peekElemOff p 5)


datasetGeotransform :: Dataset s t -> GDAL s (Maybe Geotransform)
datasetGeotransform ds = liftIO $ alloca $ \p -> do
  ret <- {#call unsafe GetGeoTransform as ^#} (unDataset ds) (castPtr p)
  if toEnumC ret == CE_None
    then liftM Just (peek p)
    else return Nothing


setDatasetGeotransform :: RWDataset s -> Geotransform -> GDAL s ()
setDatasetGeotransform ds gt = liftIO $
  checkCPLError "SetGeoTransform" $
    with gt ({#call unsafe SetGeoTransform as ^#} (unDataset ds) . castPtr)



openDatasetCount :: IO Int
openDatasetCount =
  alloca $ \ppDs ->
  alloca $ \pCount -> do
    {#call unsafe GetOpenDatasets as ^#} ppDs pCount
    liftM fromIntegral (peek pCount)

metadataDomains :: MajorObject o t => o t -> GDAL s [Text]
#if SUPPORTS_METADATA_DOMAINS
metadataDomains o =
  liftIO $
  fromCPLStringList $
  {#call unsafe GetMetadataDomainList as ^#} (majorObject o)
#else
metadataDomains = const (return [])
#endif

metadata
  :: MajorObject o t
  => Maybe ByteString -> o t -> GDAL s [(Text,Text)]
metadata domain o =
  liftIO $
  withMaybeByteString domain
  ({#call unsafe GetMetadata as ^#} (majorObject o)
    >=> liftM (map breakIt) . fromBorrowedCPLStringList)
  where
    breakIt s =
      case T.break (=='=') s of
        (k,"") -> (k,"")
        (k, v) -> (k, T.tail v)

metadataItem
  :: MajorObject o t
  => Maybe ByteString -> ByteString -> o t -> GDAL s (Maybe ByteString)
metadataItem domain key o =
  liftIO $
  useAsCString key $ \pKey ->
  withMaybeByteString domain $
    {#call unsafe GetMetadataItem as ^#} (majorObject o) pKey >=>
      maybePackCString

setMetadataItem
  :: (MajorObject o t, t ~ ReadWrite)
  => Maybe ByteString -> ByteString -> ByteString -> o t -> GDAL s ()
setMetadataItem domain key val o =
  liftIO $
  checkCPLError "setMetadataItem" $
  useAsCString key $ \pKey ->
  useAsCString val $ \pVal ->
  withMaybeByteString domain $
    {#call unsafe GDALSetMetadataItem as ^#} (majorObject o) pKey pVal

description :: MajorObject o t => o t -> GDAL s ByteString
description =
  liftIO . ({#call unsafe GetDescription as ^#} . majorObject >=> packCString)

setDescription
  :: (MajorObject o t, t ~ ReadWrite)
  => ByteString -> o t -> GDAL s ()
setDescription val =
  liftIO . checkGDALCall_ const .
  useAsCString val . {#call unsafe GDALSetDescription as ^#} . majorObject
