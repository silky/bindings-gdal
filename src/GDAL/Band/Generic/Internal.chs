{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{#context lib = "gdal"#}
#include "gdal.h"

module GDAL.Band.Generic.Internal (
    Spacing
  , MajorObject (..)
  , MajorObjectH (..)
  , BandH (..)
  , RWFlag (..)
  , adviseRead
  , rasterIO
  , writeBlockH
  , readBlockH
  , bandMaskH
  , bandDataTypeH
  , getBandH
  , bandNodataValueAsDouble
  , setBandNodataValueAsDouble
) where

import GDAL.Internal.Types
import GDAL.Internal.DataType (DataType)
import GDAL.Internal.Util (fromEnumC, toEnumC)
{#import GDAL.Internal.GDAL.Types#}
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.CPLError#}

import Control.Monad (void, liftM)
import Data.Maybe (fromMaybe)

import Foreign.C.Types (CInt(..), CChar(..), CDouble(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)

adviseRead :: OptionList -> DataType -> BandH -> Envelope Int -> Size -> IO ()
adviseRead options dtype band win (XY bx by) =
  checkCPLError "adviseRead" $
  withOptionList options $
  {#call GDALRasterAdviseRead as ^#}
    band
    (fromIntegral xoff)
    (fromIntegral yoff)
    (fromIntegral sx)
    (fromIntegral sy)
    (fromIntegral bx)
    (fromIntegral by)
    (fromEnumC dtype)
  where
    !(XY sx sy)     = envelopeSize win
    !(XY xoff yoff) = envelopeMin win

type Spacing = XY Int

rasterIO
  :: DataType -> RWFlag -> BandH -> Envelope Int -> Size -> Spacing -> Ptr a
  -> IO ()
rasterIO dtype mode band win (XY bx by) (XY spx spy) ptr =
  checkCPLError "rasterIO" $
    {#call GDALRasterIO as ^#}
      band
      (fromEnumC mode)
      (fromIntegral xoff)
      (fromIntegral yoff)
      (fromIntegral sx)
      (fromIntegral sy)
      (castPtr ptr)
      (fromIntegral bx)
      (fromIntegral by)
      (fromEnumC dtype)
      (fromIntegral spx)
      (fromIntegral spy)
  where
    !(XY sx sy)     = envelopeSize win
    !(XY xoff yoff) = envelopeMin win

readBlockH :: BandH -> BlockIx -> Ptr a -> IO ()
readBlockH band blockIx ptr =
  --checkCPLError "readBlock" $
  void $
  {#call GDALReadBlock as ^#}
    band
    (px bi)
    (py bi)
    (castPtr ptr)
  where
    !bi = fmap fromIntegral blockIx
{-# INLINE readBlockH #-}

writeBlockH :: BandH -> BlockIx -> Ptr a -> IO ()
writeBlockH band blockIx ptr =
  checkCPLError "writeBlock" $
  {#call GDALWriteBlock as ^#}
    band
    (px bi)
    (py bi)
    (castPtr ptr)
  where
    !bi = fmap fromIntegral blockIx

getBandH :: Int -> DatasetH -> IO BandH
getBandH n ds =
  checkGDALCall checkit $
  {#call GDALGetRasterBand as ^#} ds (fromIntegral n)
  where
    checkit exc p
      | p == nullBandH = Just (fromMaybe (GDALBindingException NullBand) exc)
      | otherwise      = Nothing

bandMaskH :: BandH -> IO BandH
bandMaskH = {#call GDALGetMaskBand as ^#}

bandDataTypeH :: BandH -> DataType
bandDataTypeH = toEnumC . {#call pure unsafe GDALGetRasterDataType as ^#}
{-# INLINE bandDataTypeH #-}

bandNodataValueAsDouble :: BandH -> IO (Maybe Double)
bandNodataValueAsDouble b =
  alloca $ \p -> do
    value <- {#call unsafe GDALGetRasterNoDataValue as ^#} b p
    hasNodata <- liftM toBool $ peek p
    return (if hasNodata then Just (realToFrac value) else Nothing)


setBandNodataValueAsDouble :: BandH -> Double -> IO ()
setBandNodataValueAsDouble b v =
  checkCPLError "setBandNodataValueAsDouble" $
  {#call unsafe GDALSetRasterNoDataValue as ^#} b (realToFrac v)
