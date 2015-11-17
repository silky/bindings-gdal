{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GDAL.Band.Masked.Base (
    Band
  , MVector
  , Vector
  , IOVector
  , STVector
  , VM.Nullable (..)
  , VM.Mask (..)
  , baseBand
) where

import GDAL.Internal.Types
import GDAL.Internal.DataType (GDALType, DataType(GDT_Byte))
import GDAL.Internal.GDAL.Types (
    GDALRasterException(BandDoesNotAllowNoData, InvalidBlockSize)
  , MaskType(..)
  )
import GDAL.Internal.CPLError (throwBindingException)
import qualified GDAL.Band.Generic as BG
import GDAL.Band.Generic.Internal
import qualified GDAL.Internal.Vector.Masked as VM

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as M
import qualified Data.Vector.Storable          as St
import qualified Data.Vector.Storable.Mutable  as Stm
import qualified Data.Vector.Fusion.Bundle     as Bundle

import Foreign.Ptr (Ptr)

import GHC.Exts (IsList(..), inline)

import Text.Read ( Read(..), readListPrecDefault )



newtype Band (b :: * -> * -> AccessMode -> *) s a (t::AccessMode) =
  Band (b s (VM.Elem a) t)

newtype instance BG.MVector (Band b) s a =
  MV_Band (VM.MVector (BG.MVector b) s a)
newtype instance BG.Vector  (Band b)   a =
  V_Band  (VM.Vector  (BG.Vector  b)   a)

type MVector b = BG.MVector (Band b)
type Vector  b = BG.Vector  (Band b)

type IOVector b = BG.IOVector (Band b)
type STVector b = BG.STVector (Band b)


instance (VM.Nullable a , M.MVector (BG.MVector b) (VM.Elem a)) => M.MVector (MVector b) a where
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


instance ( VM.Nullable a
         , M.MVector (BG.MVector b) (VM.Elem a)
         , G.Vector (BG.Vector b) (VM.Elem a)
         ) => G.Vector (Vector b) a where
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

instance (Eq a, G.Vector (Vector b) a) => Eq (Vector b a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))

instance (Ord a, G.Vector (Vector b) a) => Ord (Vector b a) where
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

instance (VM.Nullable a, G.Vector (BG.Vector b) (VM.Elem a)) => Monoid (Vector b a) where
  {-# INLINE mempty #-}
  mempty = V_Band G.empty

  {-# INLINE mappend #-}
  mappend = (G.++)

  {-# INLINE mconcat #-}
  mconcat = G.concat

instance (Show a, G.Vector (Vector b) a) => Show (Vector b a) where
  showsPrec = G.showsPrec

instance (Read a, G.Vector (Vector b) a) => Read (Vector b a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault


instance (G.Vector (Vector b) a) => IsList (Vector b a) where
  type Item (Vector b a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList

instance ( VM.Nullable a , M.MVector (BG.MVector b) (VM.Elem a)
         , NFData (VM.Elem a)
         , NFData (BG.Vector b (VM.Elem a))
         )
  => NFData (Vector b a) where
  rnf (V_Band v) = rnf v

instance ( VM.Nullable a , M.MVector (BG.MVector b) (VM.Elem a)
         , NFData (VM.Elem a)
         , NFData (BG.MVector b s (VM.Elem a))
         )
  => NFData (MVector b s a) where
  rnf (MV_Band v) = rnf v


instance MajorObject (b s (VM.Elem a)) t => MajorObject (Band b s a) t where
  majorObject (Band b) = majorObject b

instance ( VM.Nullable a
         , GDALType (VM.Elem a)
         , BG.Band b s (VM.Elem a) t
         ) => BG.Band (Band b) s a t where
  bandH (Band b) = BG.bandH b
  {-# INLINE bandH #-}

  fromBandH h    = Band (BG.fromBandH h)
  {-# INLINE fromBandH #-}

  readWindow b win sz =
    case BG.bandMaskType b of
      MaskNoData -> do
        vec <- BG.readWindow (baseBand b) win sz
        nd <- noDataOrFail (baseBand b)
        return (V_Band (VM.newWithNoData nd vec))
      MaskAllValid -> do
        vec <- BG.readWindow (baseBand b) win sz
        return (V_Band (VM.newAllValid vec))

      _ -> do
        vec <- BG.readWindow (baseBand b) win sz
        liftIO $ do
          maskBand <- bandMaskH (BG.bandH b)
          mv <- M.new (sizeLen sz)
          adviseRead [] GDT_Byte maskBand win sz
          Stm.unsafeWith mv $ rasterIO GDT_Byte GF_Read maskBand win sz 0
          v <- G.unsafeFreeze mv
          return (V_Band (VM.newWithMask v vec))
  {-# INLINE readWindow #-}

  writeWindow b win sz (V_Band v) =
    case BG.bandMaskType b of
      MaskNoData -> do
        nd <- noDataOrFail (baseBand b)
        BG.writeWindow (baseBand b) win sz (VM.toBaseVectorWithNoData nd v)
      MaskAllValid ->
        maybe (throwBindingException BandDoesNotAllowNoData)
              (BG.writeWindow (baseBand b) win sz) (VM.toBaseVector v)

      _ -> do let (m, v') = VM.toBaseVectorWithMask v
              BG.writeWindow (baseBand b) win sz v'
              liftIO $ do
                mb <- bandMaskH (BG.bandH b)
                St.unsafeWith m $ rasterIO GDT_Byte GF_Write mb win sz 0
  {-# INLINE writeWindow #-}

  writeBlock b ix (V_Band v)
    | BG.bandBlockLen b /= G.length v
    = throwBindingException (InvalidBlockSize (G.length v))
    | otherwise = case BG.bandMaskType b of
        MaskNoData -> do
          noData <- noDataOrFail (baseBand b)
          BG.writeBlock (baseBand b) ix (VM.toBaseVectorWithNoData noData v)

        MaskAllValid ->
          maybe (throwBindingException BandDoesNotAllowNoData)
                (BG.writeBlock (baseBand b) ix)
                (VM.toBaseVector v)
        _ -> do let (m, v') = VM.toBaseVectorWithMask v
                BG.writeBlock (baseBand b) ix v'
                liftIO $ do
                  mb <- bandMaskH (BG.bandH b)
                  St.unsafeWith m $ maskIO GF_Write mb b ix
  {-# INLINE writeBlock #-}

  blockLoader b = do
    (buf, baseLoad) <- inline BG.blockLoader (baseBand b)
    case BG.bandMaskType b of
      MaskNoData -> do
        noData <- noDataOrFail (baseBand b)
        return (MV_Band (VM.newWithNoDataM noData buf), baseLoad)
      MaskAllValid -> do
        return (MV_Band (VM.newAllValidM buf), baseLoad)
      _ -> do
        maskBuf <- liftIO $ M.replicate (BG.bandBlockLen b) 0
        mbh <- liftIO $ bandMaskH (BG.bandH b)
        return ( MV_Band (VM.newWithMaskM maskBuf buf), \ix -> do
                  baseLoad ix
                  liftIO $ Stm.unsafeWith maskBuf $ maskIO GF_Read mbh b ix)
  {-# INLINE blockLoader #-}


maskIO
  :: BG.Band b s a t => RWFlag -> BandH -> b s a t -> BlockIx -> Ptr c -> IO ()
maskIO mode mbh b ix ptr = rasterIO GDT_Byte mode mbh win sz spacing ptr
  where
    bi  = fmap fromIntegral ix
    bs  = fmap fromIntegral (BG.bandBlockSize b)
    off = bi * bs
    rs  = fmap fromIntegral (BG.bandSize b)
    sz = liftA2 min bs (rs - off)
    win = Envelope off (off+sz)
    spacing = XY 0 (px bs)

baseBand
  :: (VM.Nullable a, BG.Band b s (VM.Elem a) t, GDALType (VM.Elem a))
  => Band b s a t -> b s (VM.Elem a) t
baseBand (Band b) = b
{-# INLINE baseBand #-}

noDataOrFail
  :: (GDALType a, BG.Band b s a t) => b s a t -> GDAL s a
noDataOrFail = liftM (fromMaybe err) . BG.bandNodataValue
  where
    err = error ("GDAL.readMasked: band has GMF_NODATA flag but did " ++
                 "not  return a nodata value")
{-# INLINE noDataOrFail #-}
