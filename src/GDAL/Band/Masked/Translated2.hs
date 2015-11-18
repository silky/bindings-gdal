{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Band.Masked.Translated2 (
    Band
  , Vector
  , MVector
  , IOVector
  , STVector
  , bandCount
  , bandDataType
  , bandBlockSize
  , bandBlockCount
  , bandBlockLen
  , bandMaskType
  , bandSize
  , bandHasOverviews
  , bandTypedAs
  , allBand
  , bandNodataValue
  , setBandNodataValue
  , getBand
  , isNative
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
import qualified GDAL.Band.Translated as T
import qualified GDAL.Band.Generic as BG

import GDAL.Internal.DataType (GDALType(dataType), DataType)
import GDAL.Internal.Types
import GDAL.Internal.GDAL.Types (
    Dataset
  , RWDataset
  , OptionList
  , ProgressFun
  , MaskType
  )


type Band     = M.Band      T.Band
type Vector   = BG.Vector   Band
type MVector  = BG.MVector  Band
type IOVector = BG.IOVector Band
type STVector = BG.STVector Band


------------------------------------------------------------------------------
-- Less polymorphic re-Exports
------------------------------------------------------------------------------


bandCount :: Dataset s t -> GDAL s Int
bandCount = BG.bandCount
{-# INLINE bandCount #-}

isNative
  :: forall s a t. (GDALType (Elem a), Nullable a)
  => Band s a t -> Bool
isNative = T.isNative . M.baseBand
{-# INLINE isNative #-}

getBand :: (GDALType (Elem a), Nullable a) => Int -> Dataset s t -> GDAL s (Band s a t)
getBand = BG.getBand
{-# INLINE getBand #-}

addBand
  :: (GDALType (Elem a), Nullable a)
  => RWDataset s -> DataType -> OptionList -> GDAL s (Band s a ReadWrite)
addBand = BG.addBand
{-# INLINE addBand #-}

fillBand :: (GDALType (Elem a), Nullable a) => a -> Band s a ReadWrite -> GDAL s ()
fillBand = BG.fillBand
{-# INLINE fillBand #-}

bandDataType :: (GDALType (Elem a), Nullable a) => Band s a t -> DataType
bandDataType = BG.bandDataType
{-# INLINE bandDataType #-}

bandBlockSize :: (GDALType (Elem a), Nullable a) => Band s a t -> Size
bandBlockSize = BG.bandBlockSize
{-# INLINE bandBlockSize #-}

bandBlockLen :: (GDALType (Elem a), Nullable a) => Band s a t -> Int
bandBlockLen = BG.bandBlockLen
{-# INLINE bandBlockLen #-}


bandSize :: (GDALType (Elem a), Nullable a) => Band s a t -> Size
bandSize = BG.bandSize
{-# INLINE bandSize #-}

allBand :: (GDALType (Elem a), Nullable a) => Band s a t -> Envelope Int
allBand = BG.allBand
{-# INLINE allBand #-}

bandBlockCount :: (GDALType (Elem a), Nullable a) => Band s a t -> XY Int
bandBlockCount = BG.bandBlockCount
{-# INLINE bandBlockCount #-}

bandHasOverviews :: (GDALType (Elem a), Nullable a) => Band s a t -> GDAL s Bool
bandHasOverviews = BG.bandHasOverviews
{-# INLINE bandHasOverviews #-}

bandTypedAs :: BG.Band b s a t => b s a t -> a -> b s a t
bandTypedAs = BG.bandTypedAs
{-# INLINE bandTypedAs #-}

bandNodataValue :: (GDALType (Elem a), Nullable a) => Band s a t -> GDAL s (Maybe (Elem a))
bandNodataValue = BG.bandNodataValue . M.baseBand
{-# INLINE bandNodataValue #-}

setBandNodataValue
  :: (GDALType (Elem a), Nullable a) => Band s a ReadWrite -> Elem a -> GDAL s ()
setBandNodataValue b = BG.setBandNodataValue (M.baseBand b)
{-# INLINE setBandNodataValue #-}

createMaskBand
  :: (GDALType (Elem a), Nullable a) => Band s a ReadWrite -> MaskType -> GDAL s ()
createMaskBand = BG.createMaskBand
{-# INLINE createMaskBand #-}



copyBand
  :: (GDALType (Elem a), Nullable a)
  => Band s a t
  -> Band s a ReadWrite
  -> OptionList
  -> Maybe ProgressFun -> GDAL s ()
copyBand = BG.copyBand
{-# INLINE copyBand #-}


writeWindow
  :: (GDALType (Elem a), Nullable a, t ~ ReadWrite)
  => Band s a t
  -> Envelope Int
  -> Size
  -> Vector a
  -> GDAL s ()
writeWindow = BG.writeWindow
{-# INLINE writeWindow #-}

readWindow
  :: (GDALType (Elem a), Nullable a)
  => Band s a t -> Envelope Int -> Size -> GDAL s (Vector a)
readWindow  = BG.readWindow
{-# INLINE readWindow #-}

readBlock :: (GDALType (Elem a), Nullable a) => Band s a t -> BlockIx -> GDAL s (Vector a)
readBlock = BG.readBlock
{-# INLINE readBlock #-}

writeBlock
  :: (GDALType (Elem a), Nullable a, t ~ ReadWrite)
  => Band s a t -> BlockIx  -> Vector a -> GDAL s ()
writeBlock = BG.writeBlock
{-# INLINE writeBlock #-}


bandMaskType :: (GDALType (Elem a), Nullable a) => Band s a t -> MaskType
bandMaskType = BG.bandMaskType
{-# INLINE bandMaskType #-}


foldl'
  :: (GDALType (Elem a), Nullable a)
  => (z -> a -> z) -> z -> Band s a t -> GDAL s z
foldl' = BG.foldl'
{-# INLINE foldl' #-}

ifoldl'
  :: (GDALType (Elem a), Nullable a)
  => (z -> XY Int -> a -> z) -> z -> Band s a t -> GDAL s z
ifoldl' = BG.ifoldl'
{-# INLINE ifoldl' #-}

foldlM'
  :: (GDALType (Elem a), Nullable a)
  => (z -> a -> GDAL s z) -> z -> Band s a t -> GDAL s z
foldlM' = BG.foldlM'
{-# INLINE foldlM' #-}

ifoldlM'
  :: (GDALType (Elem a), Nullable a)
  => (z -> XY Int -> a -> GDAL s z) -> z -> Band s a t -> GDAL s z
ifoldlM' = BG.ifoldlM'
{-# INLINE ifoldlM' #-}
