{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module GDAL.Internal.OSR (
    SpatialReference
  , CoordinateTransformation

  , srsFromWkt
  , srsFromProj4
  , srsFromEPSG
  , srsFromXML

  , srsToWkt
  , srsToProj4
  , srsToXML

  , isGeographic
  , isLocal
  , isProjected
  , isSameGeogCS
  , isSame

  , getAngularUnits
  , getLinearUnits

  , coordinateTransformation

  , cleanup
  , initialize
  , srsFromWktIO
  , withSpatialReference
  , withMaybeSRAsCString
  , withMaybeSpatialReference
  , withCoordinateTransformation
  , newSpatialRefHandle
  , newSpatialRefBorrowedHandle
  , maybeNewSpatialRefHandle
  , maybeNewSpatialRefBorrowedHandle
) where

#include "ogr_srs_api.h"

{# context lib = "gdal" prefix = "OSR" #}

import Control.Applicative ((<$>), (<*>))
import Control.Exception (catch, bracketOnError, try)
import Control.Monad (liftM, (>=>), when, void)

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..), CDouble(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.OGRError
import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.CPLString (peekCPLString)

{#pointer OGRSpatialReferenceH as SpatialReference foreign newtype#}

instance Show SpatialReference where
   show = show . srsToWkt

srsToWkt :: SpatialReference -> ByteString
srsToWkt s = exportWith fun s
  where
    fun s' p = {#call unsafe ExportToPrettyWkt as ^#} s' p 1

srsToProj4 :: SpatialReference -> ByteString
srsToProj4 = exportWith {#call unsafe ExportToProj4 as ^#}

srsToXML :: SpatialReference -> ByteString
srsToXML = exportWith fun
  where
    fun s' p = {#call unsafe ExportToXML as ^#} s' p (castPtr nullPtr)

exportWith
  :: (Ptr SpatialReference -> Ptr CString -> IO CInt)
  -> SpatialReference
  -> ByteString
exportWith fun srs =
  unsafePerformIO $
  withSpatialReference srs $ \pSrs ->
  peekCPLString $
  checkOGRError . fun pSrs
{-# NOINLINE exportWith #-}


foreign import ccall "ogr_srs_api.h &OSRRelease"
  c_release :: FunPtr (Ptr SpatialReference -> IO ())

newSpatialRefHandle
  :: IO (Ptr SpatialReference) -> IO SpatialReference
newSpatialRefHandle = maybeNewSpatialRefHandle >=> maybe exc return
  where exc = throwBindingException NullSpatialReference

maybeNewSpatialRefHandle
  :: IO (Ptr SpatialReference) -> IO (Maybe SpatialReference)
maybeNewSpatialRefHandle alloc = bracketOnError alloc freeIfNotNull go
  where
    go p
      | p==nullPtr = return Nothing
      | otherwise  = liftM (Just . SpatialReference) (newForeignPtr c_release p)
    freeIfNotNull p
      | p/=nullPtr = {#call unsafe OSRRelease as ^#} p
      | otherwise  = return ()

newSpatialRefBorrowedHandle
  :: IO (Ptr SpatialReference) -> IO SpatialReference
newSpatialRefBorrowedHandle =
  maybeNewSpatialRefBorrowedHandle >=> maybe exc return
  where exc = throwBindingException NullSpatialReference

maybeNewSpatialRefBorrowedHandle
  :: IO (Ptr SpatialReference) -> IO (Maybe SpatialReference)
maybeNewSpatialRefBorrowedHandle alloc = maybeNewSpatialRefHandle $ do
  p <- alloc
  when (p /= nullPtr) (void ({#call unsafe OSRReference as ^#} p))
  return p

emptySpatialRef :: IO SpatialReference
emptySpatialRef =
  newSpatialRefHandle ({#call unsafe NewSpatialReference as ^#} nullPtr)

srsFromWkt, srsFromProj4, srsFromXML
  :: ByteString -> Either OGRException SpatialReference
srsFromWkt = unsafePerformIO . (flip unsafeUseAsCString srsFromWktIO)
{-# NOINLINE srsFromWkt #-}

srsFromWktIO :: CString -> IO (Either OGRException SpatialReference)
srsFromWktIO a =
  (liftM Right
    (newSpatialRefHandle ({#call unsafe NewSpatialReference as ^#} a)))
  `catch` (return . Left)

srsFromProj4 = fromImporter importFromProj4
{-# NOINLINE srsFromProj4 #-}

srsFromXML = fromImporter importFromXML
{-# NOINLINE srsFromXML #-}

srsFromEPSG :: Int -> Either OGRException SpatialReference
srsFromEPSG = fromImporter importFromEPSG
{-# NOINLINE srsFromEPSG #-}

fromImporter
  :: (SpatialReference -> a -> IO CInt) -> a
  -> Either OGRException SpatialReference
fromImporter f s = unsafePerformIO $ do
  r <- emptySpatialRef
  try (checkOGRError (f r s) >> return r)
{-# NOINLINE fromImporter #-}


{#fun ImportFromProj4 as ^
   { withSpatialReference* `SpatialReference'
   , unsafeUseAsCString* `ByteString'} -> `CInt' #}

{#fun ImportFromEPSG as ^
   {withSpatialReference* `SpatialReference', `Int'} -> `CInt' #}

{#fun ImportFromXML as ^
   { withSpatialReference* `SpatialReference'
   , unsafeUseAsCString* `ByteString'} -> `CInt' #}

{#fun pure unsafe IsGeographic as ^
   {withSpatialReference* `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsLocal as ^
   {withSpatialReference* `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsProjected as ^
   {withSpatialReference* `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsSameGeogCS as ^
   { withSpatialReference* `SpatialReference'
   , withSpatialReference* `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsSame as ^
   { withSpatialReference* `SpatialReference'
   , withSpatialReference* `SpatialReference'} -> `Bool'#}

instance Eq SpatialReference where
  (==) = isSame

getLinearUnits :: SpatialReference -> (Double, String)
getLinearUnits =
  unsafePerformIO . getUnitsWith {#call unsafe OSRGetLinearUnits as ^#}

getAngularUnits :: SpatialReference -> (Double, String)
getAngularUnits =
  unsafePerformIO . getUnitsWith {#call unsafe OSRGetAngularUnits as ^#}

getUnitsWith
  :: (Ptr SpatialReference -> Ptr CString -> IO CDouble)
  -> SpatialReference
  -> IO (Double, String)
getUnitsWith fun s = alloca $ \p -> do
  value <- withSpatialReference s (\s' -> fun s' p)
  ptr <- peek p
  units <- peekCString ptr
  return (realToFrac value, units)

withMaybeSRAsCString :: Maybe SpatialReference -> (CString -> IO a) -> IO a
withMaybeSRAsCString Nothing    = ($ nullPtr)
withMaybeSRAsCString (Just srs) = unsafeUseAsCString (srsToWkt srs)

withMaybeSpatialReference
  :: Maybe SpatialReference -> (Ptr SpatialReference -> IO a) -> IO a
withMaybeSpatialReference Nothing  = ($ nullPtr)
withMaybeSpatialReference (Just s) = withSpatialReference s

{#pointer OGRCoordinateTransformationH as CoordinateTransformation
  foreign newtype#}

coordinateTransformation
  :: SpatialReference -> SpatialReference -> Maybe CoordinateTransformation
coordinateTransformation source =
  unsafePerformIO . coordinateTransformationIO source
{-# NOINLINE coordinateTransformation #-}

coordinateTransformationIO
  :: SpatialReference
  -> SpatialReference
  -> IO (Maybe CoordinateTransformation)
coordinateTransformationIO source target =
  bracketOnError alloc freeIfNotNull go
  where
    alloc =
      withSpatialReference source $ \pSource ->
      withSpatialReference target $ \pTarget ->
      {#call unsafe OCTNewCoordinateTransformation as ^#} pSource pTarget

    go p
      | p==nullPtr = return Nothing
      | otherwise  = liftM (Just . CoordinateTransformation)
                           (newForeignPtr c_destroyCT p)
    freeIfNotNull p
      | p/=nullPtr = {#call unsafe OCTDestroyCoordinateTransformation as ^#} p
      | otherwise  = return ()

foreign import ccall "ogr_srs_api.h &OCTDestroyCoordinateTransformation"
  c_destroyCT :: FunPtr (Ptr CoordinateTransformation -> IO ())

{#fun OSRCleanup as cleanup {} -> `()'#}

-- | GDAL doesn't call ogr/ogrct.cpp:LoadProj4Library in a thread-safe way
--   (at least in 1.11.2). We indirectly make sure it is called at startup
--   in the main thread (via 'withGDAL') with this function which creates
--   a dummy 'CoordinateTransformation'
initialize :: IO ()
initialize = do
  dummy <- emptySpatialRef
  void (coordinateTransformationIO dummy dummy)
