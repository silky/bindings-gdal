{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module GDAL.Band.Native (
    Band
  , MVector
  , Vector
  , IOVector
  , STVector
  , NativeType
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

import GDAL.Internal.DataType (GDALType(dataType))
import GDAL.Internal.GDAL.Types (
    GDALRasterException(..)
  , DataType
  , Dataset
  , RWDataset
  , OptionList
  , ProgressFun
  , MaskType
  )
import GDAL.Internal.CPLError (throwBindingException)
import GDAL.Internal.Types
import qualified GDAL.Band.Generic as BG
import GDAL.Band.Generic.Internal


import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Monoid   ( Monoid(..) )
import Data.Proxy (Proxy(Proxy))
import qualified Data.Vector.Storable         as St
import qualified Data.Vector.Storable.Mutable as Stm
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Generic.Mutable  as M
import qualified Data.Vector.Fusion.Bundle    as Bundle

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable)
import GHC.Exts (IsList(..), inline)

import Text.Read ( Read(..), readListPrecDefault )


newtype Band s a (t::AccessMode) = Band BandH

newtype instance BG.MVector Band s a = MV_Band (St.MVector s a)
newtype instance BG.Vector Band    a = V_Band  (St.Vector    a)

type MVector = BG.MVector Band
type Vector  = BG.Vector Band

type IOVector = BG.IOVector Band
type STVector = BG.STVector Band

type NativeType a = (GDALType a, Storable a)

instance NativeType a => M.MVector (MVector) a where
  {-# INLINE basicLength #-}
  basicLength (MV_Band v) = M.basicLength v

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MV_Band v) = MV_Band (M.basicUnsafeSlice j m v)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Band a) (MV_Band b) = M.basicOverlaps a b

  basicUnsafeNew i = liftM MV_Band (M.basicUnsafeNew i)
  {-# INLINE basicUnsafeNew #-}

#if MIN_VERSION_vector(0,11,0)
  {-# INLINE basicInitialize #-}
  basicInitialize (MV_Band v) = M.basicInitialize v
#endif

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Band v) i = inline M.basicUnsafeRead v i

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Band v) i a = M.basicUnsafeWrite v i a

  {-# INLINE basicSet #-}
  basicSet (MV_Band v) x = M.basicSet v x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Band a) (MV_Band b) = M.basicUnsafeCopy a b

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_Band a) (MV_Band b) = M.basicUnsafeMove a b



instance NativeType a => G.Vector (Vector) a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Band mv) = liftM V_Band (G.basicUnsafeFreeze mv)

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Band v) = liftM MV_Band (G.basicUnsafeThaw v)

  {-# INLINE basicLength #-}
  basicLength (V_Band v) = G.basicLength v

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice n m (V_Band v) = V_Band (G.basicUnsafeSlice n m v)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Band v) i = G.basicUnsafeIndexM v i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Band mv) (V_Band v) = G.basicUnsafeCopy mv v

  {-# INLINE elemseq #-}
  elemseq _ = seq


instance (NativeType a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))

instance (NativeType a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT

instance NativeType a => Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = V_Band St.empty

  {-# INLINE mappend #-}
  mappend = (G.++)

  {-# INLINE mconcat #-}
  mconcat = G.concat

instance (Show a, NativeType a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (Read a, NativeType a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault


instance (NativeType a) => IsList (Vector a) where
  type Item (Vector a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList

instance NativeType a => NFData (Vector a) where
  rnf (V_Band v) = rnf v

instance NativeType a => NFData (MVector s a) where
  rnf (MV_Band v) = rnf v



instance MajorObject (Band s a) t where
  majorObject (Band (BandH p)) = MajorObjectH (castPtr p)

instance NativeType a => BG.Band Band s a t where
  bandH (Band h) = h
  {-# INLINE bandH #-}

  fromBandH h    = Band h
  {-# INLINE fromBandH #-}

  validate b
    | bandDataType b == dt  = return ()
    | otherwise             = throwBindingException (NonNativeDataType dt)
    where dt = dataType (undefined :: a)
  {-# INLINE validate #-}

  readWindow b win sz =
    liftIO $ do
      vec@(MV_Band v) <- M.new (sizeLen sz)
      adviseRead [] dt (BG.bandH b) win sz
      Stm.unsafeWith v $ rasterIO dt GF_Read (BG.bandH b) win sz 0
      G.unsafeFreeze vec
    where dt = dataType (undefined :: a)
  {-# INLINE readWindow #-}

  writeWindow b win sz (V_Band v) =
    liftIO $ St.unsafeWith v $ rasterIO dt GF_Write (BG.bandH b) win sz 0
    where dt = dataType (undefined :: a)
  {-# INLINE writeWindow #-}

  writeBlock b ix (V_Band v) = do
    when (BG.bandBlockLen b /= G.length v) $
      throwBindingException (InvalidBlockSize (G.length v))
    liftIO $ St.unsafeWith v $ writeBlockH (BG.bandH b) ix
  {-# INLINE writeBlock #-}

  blockLoader b = do
    BG.validate b
    liftIO $ do
      vec@(MV_Band v) <- M.new (BG.bandBlockLen b)
      return (vec, load v)
    where load v ix = liftIO (Stm.unsafeWith v (inline readBlockH bPtr ix))
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
  :: forall s a t. NativeType a
  => Int -> Dataset s t -> GDAL s (Band s a t)
getBand = BG.getBand
{-# INLINE getBand #-}

addBand
  :: forall s a. NativeType a
  => RWDataset s -> DataType -> OptionList -> GDAL s (Band s a ReadWrite)
addBand = BG.addBand
{-# INLINE addBand #-}

fillBand :: NativeType a => a -> Band s a ReadWrite -> GDAL s ()
fillBand = BG.fillBand
{-# INLINE fillBand #-}

bandDataType :: NativeType a => Band s a t -> DataType
bandDataType = BG.bandDataType
{-# INLINE bandDataType #-}

bandBlockSize :: NativeType a => Band s a t -> Size
bandBlockSize = BG.bandBlockSize
{-# INLINE bandBlockSize #-}

bandBlockLen :: NativeType a => Band s a t -> Int
bandBlockLen = BG.bandBlockLen
{-# INLINE bandBlockLen #-}


bandSize :: NativeType a => Band s a t -> Size
bandSize = BG.bandSize
{-# INLINE bandSize #-}

allBand :: NativeType a => Band s a t -> Envelope Int
allBand = BG.allBand
{-# INLINE allBand #-}

bandBlockCount :: NativeType a => Band s a t -> XY Int
bandBlockCount = BG.bandBlockCount
{-# INLINE bandBlockCount #-}

bandHasOverviews :: NativeType a => Band s a t -> GDAL s Bool
bandHasOverviews = BG.bandHasOverviews
{-# INLINE bandHasOverviews #-}


bandNodataValue :: NativeType a => Band s a t -> GDAL s (Maybe a)
bandNodataValue = BG.bandNodataValue
{-# INLINE bandNodataValue #-}

setBandNodataValue :: NativeType a => Band s a ReadWrite -> a -> GDAL s ()
setBandNodataValue = BG.setBandNodataValue
{-# INLINE setBandNodataValue #-}

createMaskBand :: NativeType a => Band s a ReadWrite -> MaskType -> GDAL s ()
createMaskBand = BG.createMaskBand
{-# INLINE createMaskBand #-}



copyBand
  :: NativeType a
  => Band s a t
  -> Band s a ReadWrite
  -> OptionList
  -> Maybe ProgressFun -> GDAL s ()
copyBand = BG.copyBand
{-# INLINE copyBand #-}


writeWindow
  :: (NativeType a, t ~ ReadWrite)
  => Band s a t
  -> Envelope Int
  -> Size
  -> Vector a
  -> GDAL s ()
writeWindow = BG.writeWindow
{-# INLINE writeWindow #-}

readWindow
  :: NativeType a
  => Band s a t -> Envelope Int -> Size -> GDAL s (Vector a)
readWindow  = BG.readWindow
{-# INLINE readWindow #-}

readBlock :: NativeType a => Band s a t -> BlockIx -> GDAL s (Vector a)
readBlock = BG.readBlock
{-# INLINE readBlock #-}

writeBlock
  :: (NativeType a, t ~ ReadWrite)
  => Band s a t -> BlockIx  -> Vector a -> GDAL s ()
writeBlock = BG.writeBlock
{-# INLINE writeBlock #-}


bandMaskType :: NativeType a => Band s a t -> MaskType
bandMaskType = BG.bandMaskType
{-# INLINE bandMaskType #-}


foldl'
  :: NativeType a
  => (z -> a -> z) -> z -> Band s a t -> GDAL s z
foldl' = BG.foldl'
{-# INLINE foldl' #-}

ifoldl'
  :: NativeType a
  => (z -> XY Int -> a -> z) -> z -> Band s a t -> GDAL s z
ifoldl' = BG.ifoldl'
{-# INLINE ifoldl' #-}

foldlM'
  :: NativeType a
  => (z -> a -> GDAL s z) -> z -> Band s a t -> GDAL s z
foldlM' = BG.foldlM'
{-# INLINE foldlM' #-}

ifoldlM'
  :: NativeType a
  => (z -> XY Int -> a -> GDAL s z) -> z -> Band s a t -> GDAL s z
ifoldlM' = BG.ifoldlM'
{-# INLINE ifoldlM' #-}
