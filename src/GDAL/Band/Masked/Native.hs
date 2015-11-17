{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module GDAL.Band.Masked.Native (
    Band
  , MVector
  , Vector
  , IOVector
  , STVector
  , Nullable
  , GDALType
  , bandCount
  , bandDataType
  , bandBlockSize
  , bandBlockCount
  , bandBlockLen
  , bandMaskType
  , bandSize
  , bandHasOverviews
  , allBand
  , bandNodataValue
  , setBandNodataValue
  , getBand
  , addBand
  , fillBand
  , createMaskBand
  , copyBand
  , readWindow
  , writeWindow
  , readBlock
  , writeBlock
  , foldl'
  , foldlM'
  , ifoldl'
  , ifoldlM'
) where

import qualified GDAL.Band.Masked.Base as M
import GDAL.Band.Masked.Base (Nullable, Elem)
import qualified GDAL.Band.Native as N
import qualified GDAL.Band.Generic as BG

import GDAL.Internal.DataType (GDALType, DataType)
import GDAL.Internal.Types
import GDAL.Internal.GDAL.Types (
    Dataset
  , RWDataset
  , OptionList
  , ProgressFun
  , MaskType
  )


type Band = M.Band N.Band

type MVector = M.MVector N.Band
type Vector  = M.Vector  N.Band

type IOVector = M.IOVector N.Band
type STVector = M.STVector N.Band

type NativeNullableType a = (N.NativeType (Elem a), Nullable a)

------------------------------------------------------------------------------
-- Less polymorphic re-Exports
------------------------------------------------------------------------------


bandCount :: Dataset s t -> GDAL s Int
bandCount = BG.bandCount
{-# INLINE bandCount #-}

getBand :: NativeNullableType a => Int -> Dataset s t -> GDAL s (Band s a t)
getBand = BG.getBand
{-# INLINE getBand #-}

addBand
  :: NativeNullableType a
  => RWDataset s -> DataType -> OptionList -> GDAL s (Band s a ReadWrite)
addBand = BG.addBand
{-# INLINE addBand #-}

fillBand :: NativeNullableType a => a -> Band s a ReadWrite -> GDAL s ()
fillBand = BG.fillBand
{-# INLINE fillBand #-}

bandDataType :: NativeNullableType a => Band s a t -> DataType
bandDataType = BG.bandDataType
{-# INLINE bandDataType #-}

bandBlockSize :: NativeNullableType a => Band s a t -> Size
bandBlockSize = BG.bandBlockSize
{-# INLINE bandBlockSize #-}

bandBlockLen :: NativeNullableType a => Band s a t -> Int
bandBlockLen = BG.bandBlockLen
{-# INLINE bandBlockLen #-}


bandSize :: NativeNullableType a => Band s a t -> Size
bandSize = BG.bandSize
{-# INLINE bandSize #-}

allBand :: NativeNullableType a => Band s a t -> Envelope Int
allBand = BG.allBand
{-# INLINE allBand #-}

bandBlockCount :: NativeNullableType a => Band s a t -> XY Int
bandBlockCount = BG.bandBlockCount
{-# INLINE bandBlockCount #-}

bandHasOverviews :: NativeNullableType a => Band s a t -> GDAL s Bool
bandHasOverviews = BG.bandHasOverviews
{-# INLINE bandHasOverviews #-}


bandNodataValue :: NativeNullableType a => Band s a t -> GDAL s (Maybe (Elem a))
bandNodataValue = BG.bandNodataValue . M.baseBand
{-# INLINE bandNodataValue #-}

setBandNodataValue
  :: NativeNullableType a => Band s a ReadWrite -> Elem a -> GDAL s ()
setBandNodataValue b = BG.setBandNodataValue (M.baseBand b)
{-# INLINE setBandNodataValue #-}

createMaskBand
  :: NativeNullableType a => Band s a ReadWrite -> MaskType -> GDAL s ()
createMaskBand = BG.createMaskBand
{-# INLINE createMaskBand #-}



copyBand
  :: NativeNullableType a
  => Band s a t
  -> Band s a ReadWrite
  -> OptionList
  -> Maybe ProgressFun -> GDAL s ()
copyBand = BG.copyBand
{-# INLINE copyBand #-}


writeWindow
  :: (NativeNullableType a, t ~ ReadWrite)
  => Band s a t
  -> Envelope Int
  -> Size
  -> Vector a
  -> GDAL s ()
writeWindow = BG.writeWindow
{-# INLINE writeWindow #-}

readWindow
  :: NativeNullableType a
  => Band s a t -> Envelope Int -> Size -> GDAL s (Vector a)
readWindow  = BG.readWindow
{-# INLINE readWindow #-}

readBlock :: NativeNullableType a => Band s a t -> BlockIx -> GDAL s (Vector a)
readBlock = BG.readBlock
{-# INLINE readBlock #-}

writeBlock
  :: (NativeNullableType a, t ~ ReadWrite)
  => Band s a t -> BlockIx  -> Vector a -> GDAL s ()
writeBlock = BG.writeBlock
{-# INLINE writeBlock #-}


bandMaskType :: NativeNullableType a => Band s a t -> MaskType
bandMaskType = BG.bandMaskType
{-# INLINE bandMaskType #-}


foldl'
  :: NativeNullableType a
  => (z -> a -> z) -> z -> Band s a t -> GDAL s z
foldl' = BG.foldl'
{-# INLINE foldl' #-}

ifoldl'
  :: NativeNullableType a
  => (z -> XY Int -> a -> z) -> z -> Band s a t -> GDAL s z
ifoldl' = BG.ifoldl'
{-# INLINE ifoldl' #-}

foldlM'
  :: NativeNullableType a
  => (z -> a -> GDAL s z) -> z -> Band s a t -> GDAL s z
foldlM' = BG.foldlM'
{-# INLINE foldlM' #-}

ifoldlM'
  :: NativeNullableType a
  => (z -> XY Int -> a -> GDAL s z) -> z -> Band s a t -> GDAL s z
ifoldlM' = BG.ifoldlM'
{-# INLINE ifoldlM' #-}
