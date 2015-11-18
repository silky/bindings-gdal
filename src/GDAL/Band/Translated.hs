{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module GDAL.Band.Translated (
    Band
  , BG.MVector(..)
  , BG.Vector(..)
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

import qualified GDAL.Internal.DataType as DT
import GDAL.Internal.DataType (GDALType(dataType), DataType)

import GDAL.Internal.Types
import GDAL.Internal.CPLError (throwBindingException)
import GDAL.Internal.GDAL.Types (
    GDALRasterException(..)
  , Dataset
  , RWDataset
  , OptionList
  , ProgressFun
  , MaskType
  )
import qualified GDAL.Band.Generic as BG
import GDAL.Band.Generic.Internal

import Control.DeepSeq ( NFData(rnf) )
import Control.Monad (when, liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Generic.Mutable  as M

import Foreign.Ptr (Ptr, castPtr)

import Control.Monad.Primitive
import Data.Primitive.Types (Prim(..))

import GHC.Word (Word8, Word16, Word32, Word64)
import GHC.Base (Int(..), (+#))
import GHC.Exts (inline)

import Data.Primitive.ByteArray
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)

import Text.Read     ( Read(..), readListPrecDefault )

newtype Band s a (t::AccessMode) = Band (DataType, BandH)


newtype instance BG.MVector Band s a = MVector (DT.TMVector s a)
newtype instance BG.Vector Band    a = Vector (DT.TVector     a)

type MVector = BG.MVector Band
type Vector  = BG.Vector Band

type IOVector = BG.IOVector Band

instance GDALType a => M.MVector MVector a where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MVector v) = M.basicLength v
  basicUnsafeSlice i n (MVector v) = MVector $ M.basicUnsafeSlice i n v
  basicOverlaps (MVector v1) (MVector v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MVector `liftM` M.basicUnsafeNew n
  basicInitialize (MVector v) = M.basicInitialize v
  basicUnsafeReplicate n x = MVector `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MVector v) i = M.basicUnsafeRead v i
  basicUnsafeWrite (MVector v) i x = M.basicUnsafeWrite v i x
  basicClear (MVector v) = M.basicClear v
  basicSet (MVector v) x = M.basicSet v x
  basicUnsafeCopy (MVector v1) (MVector v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MVector v1) (MVector v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MVector v) n = MVector `liftM` M.basicUnsafeGrow v n

instance GDALType a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MVector v) = Vector `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (Vector v) = MVector `liftM` G.basicUnsafeThaw v
  basicLength (Vector v) = G.basicLength v
  basicUnsafeSlice i n (Vector v) = Vector $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (Vector v) i = G.basicUnsafeIndexM v i
  basicUnsafeCopy (MVector mv) (Vector v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

instance MajorObject (Band s a) t where
  majorObject (Band (_, BandH p)) = MajorObjectH (castPtr p)

instance GDALType a => BG.Band Band s a t where
  bandH (Band (_,h)) = h
  {-# INLINE bandH #-}

  fromBandH h    = Band (bandDataTypeH h, h)
  {-# INLINE fromBandH #-}

  readWindow b win sz =
    liftIO $ do
      v <- M.new (sizeLen sz)
      DT.unsafeAsNativeM v $ \dt p -> do
        adviseRead [] dt (BG.bandH b) win sz
        rasterIO dt GF_Read (BG.bandH b) win sz 0 p
      liftM Vector (G.unsafeFreeze v)
  {-# INLINE readWindow #-}

  writeWindow b win sz (Vector v) =
    liftIO $
    DT.unsafeAsNative v $ \dt -> rasterIO dt GF_Write (BG.bandH b) win sz 0
  {-# INLINE writeWindow #-}

  writeBlock b ix (Vector v) = liftIO $ do
    when (BG.bandBlockLen b /= G.length v) $
      throwBindingException (InvalidBlockSize (G.length v))
    DT.unsafeAsDataType (bandDataType b) v $ writeBlockH (BG.bandH b) ix
  {-# INLINE writeBlock #-}

  blockLoader b = do
    liftIO $ do
      v <- DT.newMVector (bandDataType b) (BG.bandBlockLen b)
      return (MVector v, load v)
    where load v ix = liftIO $ DT.unsafeAsNativeM v $
                               const (inline readBlockH bPtr ix)
          bPtr = BG.bandH b
          {-# INLINE load #-}
  {-# INLINE blockLoader #-}


------------------------------------------------------------------------------
-- Less polymorphic re-Exports
------------------------------------------------------------------------------


bandCount :: Dataset s t -> GDAL s Int
bandCount = BG.bandCount
{-# INLINE bandCount #-}

getBand
  :: GDALType a
  => Int -> Dataset s t -> GDAL s (Band s a t)
getBand = BG.getBand
{-# INLINE getBand #-}

isNative :: forall s a t. GDALType a => Band s a t -> Bool
isNative b = dataType (undefined :: a) == bandDataType b
{-# INLINE isNative #-}

addBand
  :: GDALType a
  => RWDataset s -> DataType -> OptionList -> GDAL s (Band s a ReadWrite)
addBand = BG.addBand
{-# INLINE addBand #-}

fillBand :: GDALType a => a -> Band s a ReadWrite -> GDAL s ()
fillBand = BG.fillBand
{-# INLINE fillBand #-}

bandDataType :: GDALType a => Band s a t -> DataType
bandDataType (Band (d,_)) = d
{-# INLINE bandDataType #-}

bandBlockSize :: GDALType a => Band s a t -> Size
bandBlockSize = BG.bandBlockSize
{-# INLINE bandBlockSize #-}

bandBlockLen :: GDALType a => Band s a t -> Int
bandBlockLen = BG.bandBlockLen
{-# INLINE bandBlockLen #-}


bandSize :: GDALType a => Band s a t -> Size
bandSize = BG.bandSize
{-# INLINE bandSize #-}

allBand :: GDALType a => Band s a t -> Envelope Int
allBand = BG.allBand
{-# INLINE allBand #-}

bandBlockCount :: GDALType a => Band s a t -> XY Int
bandBlockCount = BG.bandBlockCount
{-# INLINE bandBlockCount #-}

bandHasOverviews :: GDALType a => Band s a t -> GDAL s Bool
bandHasOverviews = BG.bandHasOverviews
{-# INLINE bandHasOverviews #-}


bandNodataValue :: GDALType a => Band s a t -> GDAL s (Maybe a)
bandNodataValue = BG.bandNodataValue
{-# INLINE bandNodataValue #-}

setBandNodataValue :: GDALType a => Band s a ReadWrite -> a -> GDAL s ()
setBandNodataValue = BG.setBandNodataValue
{-# INLINE setBandNodataValue #-}

createMaskBand :: GDALType a => Band s a ReadWrite -> MaskType -> GDAL s ()
createMaskBand = BG.createMaskBand
{-# INLINE createMaskBand #-}

copyBand
  :: GDALType a
  => Band s a t
  -> Band s a ReadWrite
  -> OptionList
  -> Maybe ProgressFun -> GDAL s ()
copyBand = BG.copyBand
{-# INLINE copyBand #-}

writeWindow
  :: (GDALType a, t ~ ReadWrite)
  => Band s a t
  -> Envelope Int
  -> Size
  -> Vector a
  -> GDAL s ()
writeWindow = BG.writeWindow
{-# INLINE writeWindow #-}

readWindow
  :: GDALType a
  => Band s a t -> Envelope Int -> Size -> GDAL s (Vector a)
readWindow  = BG.readWindow
{-# INLINE readWindow #-}

readBlock :: GDALType a => Band s a t -> BlockIx -> GDAL s (Vector a)
readBlock = BG.readBlock
{-# INLINE readBlock #-}

writeBlock
  :: (GDALType a, t ~ ReadWrite)
  => Band s a t -> BlockIx  -> Vector a -> GDAL s ()
writeBlock = BG.writeBlock
{-# INLINE writeBlock #-}


bandMaskType :: GDALType a => Band s a t -> MaskType
bandMaskType = BG.bandMaskType
{-# INLINE bandMaskType #-}


foldl'
  :: GDALType a
  => (z -> a -> z) -> z -> Band s a t -> GDAL s z
foldl' = BG.foldl'
{-# INLINE foldl' #-}

ifoldl'
  :: GDALType a
  => (z -> XY Int -> a -> z) -> z -> Band s a t -> GDAL s z
ifoldl' = BG.ifoldl'
{-# INLINE ifoldl' #-}

foldlM'
  :: GDALType a
  => (z -> a -> GDAL s z) -> z -> Band s a t -> GDAL s z
foldlM' = BG.foldlM'
{-# INLINE foldlM' #-}

ifoldlM'
  :: GDALType a
  => (z -> XY Int -> a -> GDAL s z) -> z -> Band s a t -> GDAL s z
ifoldlM' = BG.ifoldlM'
{-# INLINE ifoldlM' #-}
