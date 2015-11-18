{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MagicHash #-}

module GDAL.Internal.Vector.Translated (
    MVector (..)
  , Vector  (..)
  , IOVector
  , STVector

  , unsafeAsNative
  , unsafeAsNativeM
  , unsafeAsDataType
  , newMVector
) where

import GDAL.Internal.Types.Pair (Pair(..))
import GDAL.Internal.DataType (
    GDALType( gReadByteArray#
            , gWriteByteArray#
            , gIndexByteArray#
            , dataType
            )
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


import GHC.Word (Word8, Word16, Word32)
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

instance NFData (MVector s a) where
  rnf (MVector _ _ _ _) = ()

newMVector
  :: PrimMonad m
  => DataType
  -> Int
  -> m (MVector (PrimState m) a)
newMVector dt n
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
{-# INLINE newMVector #-}


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

  basicUnsafeNew = newMVector (dataType (undefined :: a))
  {-# INLINE basicUnsafeNew #-}

#if MIN_VERSION_vector(0,11,0)
  {-# INLINE basicInitialize #-}
  basicInitialize MVector{mvOff=off, mvLen=n, mvData=v, mvDataType} =
    setByteArray v (off * size) (n * size) (0 :: Word8)
    where size = sizeOfDataType (toEnum mvDataType)
#endif

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead MVector{ mvData     = MutableByteArray arr#
                         , mvOff      = I# o#
                         , mvDataType = I# dt#
                         } (I# i#) =
    primitive (gReadByteArray# dt# arr# (o# +# i#))


  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite MVector{ mvData     = MutableByteArray arr#
                          , mvOff      = I# o#
                          , mvDataType = I# dt#
                          } (I# i#) v =
    primitive_ (gWriteByteArray# dt# arr# (o# +# i#) v)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dVec@MVector{mvDataType=dDt, mvOff=i, mvLen=n, mvData=dst}
                  sVec@MVector{mvDataType=sDt, mvOff=j, mvData=src}
    | sDt == dDt = copyMutableByteArray dst (i*sz) src (j*sz) (n*sz)
    | otherwise  = loop 0
    where
      sz = sizeOfDataType (toEnum dDt)
      loop !ix
        | ix < mvLen dVec = do
            M.basicUnsafeRead sVec ix >>= M.basicUnsafeWrite dVec ix
            loop (ix+1)
        | otherwise      = return ()

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove dVec@MVector{mvDataType=dDt, mvOff=i, mvLen=n, mvData=dst}
                  sVec@MVector{mvDataType=sDt, mvOff=j, mvData=src}
    | sDt == dDt = moveByteArray dst (i*sz) src (j*sz) (n*sz)
    | otherwise  = M.basicUnsafeCopy dVec sVec
    where
      sz = sizeOfDataType (toEnum dDt)



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
  basicUnsafeIndexM Vector{ vData     = ByteArray arr#
                          , vOff      = I# o#
                          , vDataType = I# dt#
                          } (I# i#) =
    return $! gIndexByteArray# dt# arr# (o# +# i#)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dVec@MVector{mvDataType=dDt, mvOff=i, mvLen=n, mvData=dst}
                  sVec@Vector{vDataType=sDt, vOff=j, vData=src}
    | sDt == dDt = copyByteArray dst (i*sz) src (j*sz) (n*sz)
    | otherwise  = loop 0
    where
      sz = sizeOfDataType (toEnum dDt)
      loop !ix
        | ix < mvLen dVec = do
            G.basicUnsafeIndexM sVec ix >>= M.basicUnsafeWrite dVec ix
            loop (ix+1)
        | otherwise      = return ()

  {-# INLINE elemseq #-}
  elemseq _ = seq

instance NFData (Vector a) where
  rnf (Vector _ _ _ _) = ()



unsafeAsNative :: Vector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeAsNative Vector{vData, vDataType, vOff} f = do
  r <- f (toEnum vDataType) (Ptr addr)
  touch vData
  return r
  where !(Addr addr) = byteArrayContents vData `plusAddr` (vOff*size)
        size = sizeOfDataType (toEnum vDataType)
{-# INLINE unsafeAsNative #-}

unsafeAsNativeM
  :: IOVector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeAsNativeM MVector{mvData, mvOff, mvDataType} f = do
  r <- f (toEnum mvDataType) (Ptr addr)
  touch mvData
  return r
  where !(Addr addr) = mutableByteArrayContents mvData `plusAddr` (mvOff*size)
        size = sizeOfDataType (toEnum mvDataType)
{-# INLINE unsafeAsNativeM #-}

unsafeAsDataType
  :: GDALType a => DataType -> Vector a -> (Ptr () -> IO b) -> IO b
unsafeAsDataType dt v f
  | fromEnum dt == vDataType v = unsafeAsNative v (const f)
  | otherwise = do
      copy <- newMVector dt (vLen v)
      G.basicUnsafeCopy copy v
      unsafeAsNativeM copy (const f)
{-# INLINE unsafeAsDataType #-}
