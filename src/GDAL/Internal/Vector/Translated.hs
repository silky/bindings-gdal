{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module GDAL.Internal.Vector.Translated (
    MVector (..)
  , Vector  (..)
  , IOVector
  , STVector

  , unsafeWith
  , unsafeWithM
  , unsafeWithAsDataType
  , newTMVector
) where

import GDAL.Internal.Types.Pair (Pair(..))
import GDAL.Internal.DataType (
    GDALType(..)
  , DataType(..)
  , sizeOfDataType
  )

import Control.DeepSeq ( NFData(rnf) )
import Control.Monad (liftM)
import Control.Monad.Primitive

import qualified Data.Vector.Generic.Mutable  as M
import qualified Data.Vector.Generic          as G

import Data.Primitive.Types
import Data.Primitive.ByteArray
import Data.Primitive.Addr
import Data.Typeable (Typeable)

import Foreign.Ptr (Ptr)


import GHC.Word (Word8, Word16, Word32, Word64)
import GHC.Int (Int16, Int32)
import GHC.Base
import GHC.Ptr (Ptr(..))




data MVector s a =
  MVector { mvLen      :: {-# UNPACK #-} !Int
          , mvOff      :: {-# UNPACK #-} !Int
          , mvDataType :: {-# UNPACK #-} !Int
          , mvData     :: {-# UNPACK #-} !(MutableByteArray s)
          }
  deriving ( Typeable )

newTMVector
  :: PrimMonad m
  => DataType
  -> Int
  -> m (MVector (PrimState m) a)
newTMVector dt n
  | n < 0 = error $ "GDAL.Vector.new: negative length: " ++ show n
  | n > mx = error $ "GDAL.Vector.new: length too large: " ++ show n
  | otherwise = do
      arr <- newPinnedByteArray (n*size)
      return MVector
          { mvLen      = n
          , mvOff      = 0
          , mvDataType = fromEnum dt
          , mvData     = arr
          }
  where
    mx = maxBound `quot` (sizeOfDataType dt) :: Int
    size = sizeOfDataType dt
{-# INLINE newTMVector #-}


type IOVector = MVector RealWorld
type STVector s = MVector s

instance GDALType a => M.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength v = mvLen v

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m v = v { mvOff = mvOff v + j
                             , mvLen = m
                             }

  {-# INLINE basicOverlaps #-}
  basicOverlaps MVector{mvOff=i, mvLen=m, mvData=arr1}
                MVector{mvOff=j, mvLen=n, mvData=arr2}
    = sameMutableByteArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

  basicUnsafeNew = newTMVector (dataType (undefined :: a))
  {-# INLINE basicUnsafeNew #-}

#if MIN_VERSION_vector(0,11,0)
  {-# INLINE basicInitialize #-}
  basicInitialize MVector{mvOff=off, mvLen=n, mvData=v, mvDataType} =
    setByteArray v (off * size) (n * size) (0 :: Word8)
    where size = sizeOfDataType (toEnum mvDataType)
#endif

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead MVector{mvData, mvOff, mvDataType} i
    | mvDataType == fromEnum GDT_Byte
    = (gFromIntegral :: Word8 -> a) `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_UInt16
    = (gFromIntegral :: Word16 -> a) `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_UInt32
    = (gFromIntegral :: Word32 -> a) `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_Int16
    = (gFromIntegral :: Int16 -> a) `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_Int32
    = (gFromIntegral :: Int32 -> a) `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_Float32
    = (gFromReal :: Float -> a) `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_Float64
    = (gFromReal :: Double -> a) `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_CInt16
    = (gFromIntegralPair :: Pair Int16 -> a)
        `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_CInt32
    = (gFromIntegralPair :: Pair Int32 -> a)
        `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_CFloat32
    = (gFromRealPair :: Pair Float -> a)
        `liftM` readByteArray mvData (mvOff+i)
    | mvDataType == fromEnum GDT_CFloat64
    = (gFromRealPair :: Pair Double -> a)
        `liftM` readByteArray mvData (mvOff+i)
    | otherwise = error "GDAL.Internal.Vector.Translated.basicUnsafeRead"

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite MVector{ mvData , mvOff , mvDataType } i x
    | mvDataType == fromEnum GDT_Byte
    = writeByteArray mvData (mvOff+i) (gToIntegral x :: Word8)
    | mvDataType == fromEnum GDT_UInt16
    = writeByteArray mvData (mvOff+i) (gToIntegral x :: Word16)
    | mvDataType == fromEnum GDT_UInt32
    = writeByteArray mvData (mvOff+i) (gToIntegral x :: Word32)
    | mvDataType == fromEnum GDT_Int16
    = writeByteArray mvData (mvOff+i) (gToIntegral x :: Int16)
    | mvDataType == fromEnum GDT_Int32
    = writeByteArray mvData (mvOff+i) (gToIntegral x :: Int32)
    | mvDataType == fromEnum GDT_Float32
    = writeByteArray mvData (mvOff+i) (gToReal x :: Float)
    | mvDataType == fromEnum GDT_Float64
    = writeByteArray mvData (mvOff+i) (gToReal x :: Double)
    | mvDataType == fromEnum GDT_CInt16
    = writeByteArray mvData (mvOff+i) (gToIntegralPair x :: Pair Int16)
    | mvDataType == fromEnum GDT_CInt32
    = writeByteArray mvData (mvOff+i) (gToIntegralPair x :: Pair Int32)
    | mvDataType == fromEnum GDT_CFloat32
    = writeByteArray mvData (mvOff+i) (gToRealPair x :: Pair Float)
    | mvDataType == fromEnum GDT_CFloat64
    = writeByteArray mvData (mvOff+i) (gToRealPair x :: Pair Double)
    | otherwise = error "GDAL.Internal.Vector.Translated.basicUnsafeWrite"

  {-# INLINE basicSet #-}
  basicSet = tVectorSet

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dVec sVec = loop 0
    where
      loop !ix
        | ix < mvLen dVec = do
            M.basicUnsafeRead sVec ix >>= M.basicUnsafeWrite dVec ix
            loop (ix+1)
        | otherwise      = return ()

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove = M.basicUnsafeCopy


tVectorSet
  :: forall m a. (GDALType a, PrimMonad m)
  => MVector (PrimState m) a -> a -> m ()
tVectorSet v@MVector{mvLen=n, mvData, mvDataType} x
  | n == 0 = return ()
  | otherwise = case sizeOfDataType (toEnum mvDataType) of
                  1 -> setAsPrim (undefined :: Word8)
                  2 -> setAsPrim (undefined :: Word16)
                  4 -> setAsPrim (undefined :: Word32)
                  8 -> setAsPrim (undefined :: Word64)
                  _ -> let do_set !i
                             | i<n = M.basicUnsafeWrite v i x  >> do_set (i+1)
                             | otherwise = return ()
                       in do_set 0

  where
    {-# INLINE[0] setAsPrim #-}
    setAsPrim :: Prim b => b -> m ()
    setAsPrim y = do
      M.basicUnsafeWrite v 0 x
      w <- readByteArray mvData 0
      setByteArray mvData 1 (n-1) (w `asTypeOf` y)
{-# INLINE tVectorSet #-}


unsafeWithM
  :: IOVector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeWithM MVector{mvData, mvOff, mvDataType} f = do
  r <- f (toEnum mvDataType) (Ptr addr)
  touch mvData
  return r
  where !(Addr addr) = mutableByteArrayContents mvData `plusAddr` (mvOff*size)
        size = sizeOfDataType (toEnum mvDataType)
{-# INLINE unsafeWithM #-}


data Vector a =
  Vector { vLen      :: {-# UNPACK #-} !Int
         , vOff      :: {-# UNPACK #-} !Int
         , vDataType :: {-# UNPACK #-} !Int
         , vData     :: {-# UNPACK #-} !ByteArray
         }
  deriving ( Typeable )

type instance G.Mutable Vector = MVector

instance GDALType a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze mv = do
    arr <- unsafeFreezeByteArray (mvData mv)
    return $ Vector { vLen      = mvLen mv
                    , vOff      = mvOff mv
                    , vDataType = mvDataType mv
                    , vData     = arr
                    }

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw v = do
    arr <- unsafeThawByteArray (vData v)
    return $ MVector { mvLen      = vLen v
                     , mvOff      = vOff v
                     , mvDataType = vDataType v
                     , mvData     = arr
                     }

  {-# INLINE basicLength #-}
  basicLength v = vLen v

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m v = v { vOff = vOff v + j
                             , vLen = m
                             }

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM Vector{vOff,vData,vDataType} i
    | vDataType == fromEnum GDT_Byte
    = return $! gFromIntegral (indexByteArray vData (vOff+i) :: Word8)
    | vDataType == fromEnum GDT_UInt16
    = return $! gFromIntegral (indexByteArray vData (vOff+i) :: Word16)
    | vDataType == fromEnum GDT_UInt32
    = return $! gFromIntegral (indexByteArray vData (vOff+i) :: Word32)
    | vDataType == fromEnum GDT_Int16
    = return $! gFromIntegral (indexByteArray vData (vOff+i) :: Int16)
    | vDataType == fromEnum GDT_Int32
    = return $! gFromIntegral (indexByteArray vData (vOff+i) :: Int32)
    | vDataType == fromEnum GDT_Float32
    = return $! gFromReal (indexByteArray vData (vOff+i) :: Float)
    | vDataType == fromEnum GDT_Float64
    = return $! gFromReal (indexByteArray vData (vOff+i) :: Double)
    | vDataType == fromEnum GDT_CInt16
    = return $! gFromIntegralPair (indexByteArray vData (vOff+i) :: Pair Int16)
    | vDataType == fromEnum GDT_CInt32
    = return $! gFromIntegralPair (indexByteArray vData (vOff+i) :: Pair Int32)
    | vDataType == fromEnum GDT_CFloat32
    = return $! gFromRealPair (indexByteArray vData (vOff+i) :: Pair Float)
    | vDataType == fromEnum GDT_CFloat64
    = return $! gFromRealPair (indexByteArray vData (vOff+i) :: Pair Double)
    | otherwise = error "GDAL.Internal.Vector.Translated.basicUnsafeIndex"

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dst src = G.unsafeThaw src >>= M.unsafeCopy dst

  {-# INLINE elemseq #-}
  elemseq _ = seq


unsafeWith :: Vector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeWith Vector{vData, vDataType, vOff} f = do
  r <- f (toEnum vDataType) (Ptr addr)
  touch vData
  return r
  where !(Addr addr) = byteArrayContents vData `plusAddr` (vOff*size)
        size = sizeOfDataType (toEnum vDataType)
{-# INLINE unsafeWith #-}

unsafeWithAsDataType
  :: GDALType a => DataType -> Vector a -> (Ptr () -> IO b) -> IO b
unsafeWithAsDataType dt v f
  | fromEnum dt == vDataType v = unsafeWith v (const f)
  | otherwise = do
      copy <- newTMVector dt (vLen v)
      G.basicUnsafeCopy copy v
      unsafeWithM copy (const f)
{-# INLINE unsafeWithAsDataType #-}
