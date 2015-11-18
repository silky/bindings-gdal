{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

module GDAL.Band.Masked.Translated (
    Band
  , MVector
  , Vector
  , IOVector
  , STVector
  , MaskedTranslated
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

import GDAL.Internal.Types
import GDAL.Internal.GDAL.Types (
    Dataset
  , RWDataset
  , OptionList
  , ProgressFun
  , MaskType
  )
import GDAL.Internal.Types.Value
import GDAL.Internal.DataType (
    GDALType ( toUVector )
  , DataType (GDT_Byte)
  , convertGType
  )
import qualified GDAL.Internal.Vector.Translated as T
import qualified GDAL.Internal.Vector.Masked as VM
import qualified GDAL.Internal.DataType as DT
import GDAL.Internal.GDAL.Types (
    GDALRasterException(BandDoesNotAllowNoData, InvalidBlockSize)
  , MaskType(..)
  )
import GDAL.Internal.CPLError (throwBindingException)
import qualified GDAL.Band.Generic as BG
import GDAL.Band.Generic.Internal

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
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector.Fusion.Bundle     as Bundle

import Foreign.Ptr (Ptr, castPtr)

import GHC.Exts (IsList(..), inline)

import Text.Read ( Read(..), readListPrecDefault )


newtype Band s a (t::AccessMode) = Band BandH


newtype instance BG.MVector Band s a = MVector (U.MVector s a)
newtype instance BG.Vector Band    a = Vector (U.Vector     a)

type MVector = BG.MVector Band
type Vector  = BG.Vector Band

type IOVector = BG.IOVector Band
type STVector = BG.STVector Band

instance GDALType a => M.MVector MVector (Value a) where
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

instance GDALType a => G.Vector Vector (Value a) where
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
  majorObject (Band (BandH p)) = MajorObjectH (castPtr p)


type MaskedTranslated a =
  ( Eq a
  , GDALType a
  --, BG.Band Band s (Value a) t
  , DT.Vector a ~ VM.Vector T.Vector)


instance (Eq a, MaskedTranslated a) => Eq (Vector (Value a)) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))

instance (Ord a, MaskedTranslated a) => Ord (Vector (Value a)) where
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

instance MaskedTranslated a => Monoid (Vector (Value a)) where
  {-# INLINE mempty #-}
  mempty = G.empty

  {-# INLINE mappend #-}
  mappend = (G.++)

  {-# INLINE mconcat #-}
  mconcat = G.concat

instance (Show a, MaskedTranslated a) => Show (Vector (Value a)) where
  showsPrec = G.showsPrec

instance (Read a, MaskedTranslated a) => Read (Vector (Value a)) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault


instance MaskedTranslated a => IsList (Vector (Value a)) where
  type Item (Vector (Value a)) = Value a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList


instance (Eq a, GDALType a, DT.Vector a ~ VM.Vector T.Vector)
  => BG.Band Band s (Value a) t where
  bandH (Band h) = h
  {-# INLINE bandH #-}

  fromBandH h    = Band h
  {-# INLINE fromBandH #-}


  readWindow b win sz = do
    vec <- liftIO $ do
      v <- M.new (sizeLen sz)
      T.unsafeAsNativeM v $ \dt p -> do
        adviseRead [] dt (BG.bandH b) win sz
        rasterIO dt GF_Read (BG.bandH b) win sz 0 p
      G.unsafeFreeze v
    case BG.bandMaskType b of
      MaskNoData -> do
        nd <- noDataOrFail b
        return (Vector (toUVector (VM.newWithNoData nd vec)))
      MaskAllValid -> do
        return (Vector (toUVector (VM.newAllValid vec)))

      _ -> do
        liftIO $ do
          maskBand <- bandMaskH (BG.bandH b)
          mv <- M.new (sizeLen sz)
          adviseRead [] GDT_Byte maskBand win sz
          Stm.unsafeWith mv $ rasterIO GDT_Byte GF_Read maskBand win sz 0
          v <- G.unsafeFreeze mv
          return (Vector (toUVector (VM.newWithMask v vec)))
  {-# INLINE readWindow #-}

  writeWindow b win sz (Vector (DT.fromUVector -> v)) =
    case BG.bandMaskType b of
      MaskNoData -> do
        nd <- noDataOrFail b
        writeT (VM.toBaseVectorWithNoData nd v)
      MaskAllValid ->
        maybe (throwBindingException BandDoesNotAllowNoData)
              (writeT) (VM.toBaseVector v)

      _ -> do let (m, v') = VM.toBaseVectorWithMask v
              writeT v'
              liftIO $ do
                mb <- bandMaskH (BG.bandH b)
                St.unsafeWith m $ rasterIO GDT_Byte GF_Write mb win sz 0
    where
      writeT vt = liftIO $ T.unsafeAsNative vt $ \dt ->
                    rasterIO dt GF_Write (BG.bandH b) win sz 0
  {-# INLINE writeWindow #-}

  writeBlock b ix (Vector (DT.fromUVector -> v))
    | BG.bandBlockLen b /= G.length v
    = throwBindingException (InvalidBlockSize (G.length v))
    | otherwise = case BG.bandMaskType b of
        MaskNoData -> do
          noData <- noDataOrFail b
          writeT (VM.toBaseVectorWithNoData noData v)

        MaskAllValid ->
          maybe (throwBindingException BandDoesNotAllowNoData)
                writeT (VM.toBaseVector v)
        _ -> do let (m, v') = VM.toBaseVectorWithMask v
                writeT v'
                liftIO $ do
                  mb <- bandMaskH (BG.bandH b)
                  St.unsafeWith m $ maskIO GF_Write mb b ix
    where writeT vt = liftIO $ T.unsafeAsDataType (BG.bandDataType b) vt $
                      writeBlockH (BG.bandH b) ix
  {-# INLINE writeBlock #-}

  blockLoader b@(Band bPtr) = do
    vt <- liftIO $ T.newMVector (BG.bandDataType b) (BG.bandBlockLen b)
    case BG.bandMaskType b of
      MaskNoData -> do
        noData <- noDataOrFail b
        let uvec = DT.toUMVector (VM.newWithNoDataM noData vt)
        return (MVector uvec, loadVt vt)
      MaskAllValid -> do
        let uvec = DT.toUMVector (VM.newAllValidM vt)
        return (MVector uvec, loadVt vt)
      _ -> do
        maskBuf <- liftIO $ M.replicate (BG.bandBlockLen b) 0
        mbh <- liftIO $ bandMaskH bPtr
        let uvec = DT.toUMVector (VM.newWithMaskM maskBuf vt)
        return ( MVector uvec, \ix -> do
                  loadVt vt ix
                  liftIO $ Stm.unsafeWith maskBuf $ maskIO GF_Read mbh b ix)
    where loadVt v ix = liftIO $ T.unsafeAsNativeM v $
                                const (inline readBlockH bPtr ix)
  {-# INLINE blockLoader #-}

maskIO
  :: (MaskedTranslated a, BG.Band Band s (Value a) t)
  => RWFlag -> BandH -> Band s (Value a) t -> BlockIx -> Ptr c -> IO ()
maskIO mode mbh b ix ptr = rasterIO GDT_Byte mode mbh win sz spacing ptr
  where
    bi  = fmap fromIntegral ix
    bs  = fmap fromIntegral (BG.bandBlockSize b)
    off = bi * bs
    rs  = fmap fromIntegral (BG.bandSize b)
    sz = liftA2 min bs (rs - off)
    win = Envelope off (off+sz)
    spacing = XY 0 (px bs)
{-# INLINE maskIO #-}


noDataOrFail
  :: (MaskedTranslated a, BG.Band Band s (Value a) t)
  => Band s (Value a) t -> GDAL s a
noDataOrFail = liftM (maybe err id) . bandNodataValue
  where
    err = error ("GDAL.readMasked: band has GMF_NODATA flag but did " ++
                 "not  return a nodata value")
{-# INLINE noDataOrFail #-}

------------------------------------------------------------------------------
-- Less polymorphic re-Exports
------------------------------------------------------------------------------


bandCount :: Dataset s t -> GDAL s Int
bandCount = BG.bandCount
{-# INLINE bandCount #-}


getBand :: MaskedTranslated a => Int -> Dataset s t -> GDAL s (Band s (Value a) t)
getBand = BG.getBand
{-# INLINE getBand #-}

addBand
  :: MaskedTranslated a
  => RWDataset s -> DataType -> OptionList -> GDAL s (Band s (Value a) ReadWrite)
addBand = BG.addBand
{-# INLINE addBand #-}

fillBand
  :: MaskedTranslated a
  => Value a -> Band s (Value a) ReadWrite -> GDAL s ()
fillBand = BG.fillBand
{-# INLINE fillBand #-}

bandDataType :: MaskedTranslated a => Band s (Value a) t -> DataType
bandDataType = BG.bandDataType
{-# INLINE bandDataType #-}

bandBlockSize :: MaskedTranslated a => Band s (Value a) t -> Size
bandBlockSize = BG.bandBlockSize
{-# INLINE bandBlockSize #-}

bandBlockLen :: MaskedTranslated a => Band s (Value a) t -> Int
bandBlockLen = BG.bandBlockLen
{-# INLINE bandBlockLen #-}


bandSize :: MaskedTranslated a => Band s (Value a) t -> Size
bandSize = BG.bandSize
{-# INLINE bandSize #-}

allBand :: MaskedTranslated a => Band s (Value a) t -> Envelope Int
allBand = BG.allBand
{-# INLINE allBand #-}

bandBlockCount :: MaskedTranslated a => Band s (Value a) t -> XY Int
bandBlockCount = BG.bandBlockCount
{-# INLINE bandBlockCount #-}

bandHasOverviews :: MaskedTranslated a => Band s (Value a) t -> GDAL s Bool
bandHasOverviews = BG.bandHasOverviews
{-# INLINE bandHasOverviews #-}

bandTypedAs :: BG.Band b s a t => b s a t -> a -> b s a t
bandTypedAs = BG.bandTypedAs
{-# INLINE bandTypedAs #-}

bandNodataValue :: MaskedTranslated a => Band s (Value a) t -> GDAL s (Maybe a)
bandNodataValue (Band b) =
  liftM (fmap convertGType) (liftIO (bandNodataValueAsDouble b))
{-# INLINE bandNodataValue #-}

setBandNodataValue
  :: MaskedTranslated a
  => Band s (Value a) ReadWrite -> a -> GDAL s ()
setBandNodataValue (Band b) =
  liftIO . setBandNodataValueAsDouble b . convertGType
{-# INLINE setBandNodataValue #-}

createMaskBand
  :: MaskedTranslated a
  => Band s (Value a) ReadWrite -> MaskType -> GDAL s ()
createMaskBand = BG.createMaskBand
{-# INLINE createMaskBand #-}



copyBand
  :: MaskedTranslated a
  => Band s (Value a) t
  -> Band s (Value a) ReadWrite
  -> OptionList
  -> Maybe ProgressFun -> GDAL s ()
copyBand = BG.copyBand
{-# INLINE copyBand #-}


writeWindow
  :: (MaskedTranslated a, t ~ ReadWrite)
  => Band s (Value a) t
  -> Envelope Int
  -> Size
  -> Vector (Value a)
  -> GDAL s ()
writeWindow = BG.writeWindow
{-# INLINE writeWindow #-}

readWindow
  :: MaskedTranslated a
  => Band s (Value a) t -> Envelope Int -> Size -> GDAL s (Vector (Value a))
readWindow  = BG.readWindow
{-# INLINE readWindow #-}

readBlock :: MaskedTranslated a => Band s (Value a) t -> BlockIx -> GDAL s (Vector (Value a))
readBlock = BG.readBlock
{-# INLINE readBlock #-}

writeBlock
  :: (MaskedTranslated a, t ~ ReadWrite)
  => Band s (Value a) t -> BlockIx  -> Vector (Value a) -> GDAL s ()
writeBlock = BG.writeBlock
{-# INLINE writeBlock #-}


bandMaskType :: MaskedTranslated a => Band s (Value a) t -> MaskType
bandMaskType = BG.bandMaskType
{-# INLINE bandMaskType #-}


foldl'
  :: MaskedTranslated a
  => (z -> Value a -> z) -> z -> Band s (Value a) t -> GDAL s z
foldl' = BG.foldl'
{-# INLINE foldl' #-}

ifoldl'
  :: MaskedTranslated a
  => (z -> XY Int -> Value a -> z) -> z -> Band s (Value a) t -> GDAL s z
ifoldl' = BG.ifoldl'
{-# INLINE ifoldl' #-}

foldlM'
  :: MaskedTranslated a
  => (z -> Value a -> GDAL s z) -> z -> Band s (Value a) t -> GDAL s z
foldlM' = BG.foldlM'
{-# INLINE foldlM' #-}

ifoldlM'
  :: MaskedTranslated a
  => (z -> XY Int -> Value a -> GDAL s z) -> z -> Band s (Value a) t -> GDAL s z
ifoldlM' = BG.ifoldlM'
{-# INLINE ifoldlM' #-}
