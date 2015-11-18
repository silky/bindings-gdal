{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NamedFieldPuns #-}

#include "bindings.h"

module GDAL.Internal.DataType (
    GDALType (..)
  , DataType
  , TMVector
  , TVector

  , sizeOfDataType

  , newMVector
  , unsafeAsNative
  , unsafeAsNativeM
  , unsafeAsDataType

  , newWithMask
  , newWithMaskM
  , newWithNoData
  , newWithNoDataM
  , newAllValid
  , newAllValidM
  , toBaseVector
  , toBaseVectorWithNoData
  , toBaseVectorWithMask

  , gdtByte
  , gdtUInt16
  , gdtUInt32
  , gdtInt16
  , gdtInt32
  , gdtFloat32
  , gdtFloat64
  , gdtCInt16
  , gdtCInt32
  , gdtCFloat32
  , gdtCFloat64
  , gdtUnknown
) where

import GDAL.Internal.Types.Pair
import GDAL.Internal.Types.Value

import Control.Monad.Primitive
import Control.Monad.ST (runST)
import Control.DeepSeq ( NFData(rnf) )
import Control.Monad (liftM)

import Data.Int (Int8, Int16, Int32)
import Data.Primitive.Types
import Data.Primitive.MachDeps
import Data.Primitive.ByteArray
import Data.Primitive.Addr
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32)

import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as M
import qualified Data.Vector.Storable          as St
import qualified Data.Vector.Storable.Mutable  as Stm
import qualified Data.Vector.Unboxed           as U

import Foreign.Ptr (Ptr)

import GHC.Base (MutableByteArray#, ByteArray#, State#, Int#, Int(..))
import GHC.Word (Word8, Word16, Word32)
import GHC.Int (Int16, Int32)
import GHC.Base
import GHC.Ptr (Ptr(..))


import Unsafe.Coerce (unsafeCoerce)

------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------
class Eq a => GDALType a where
  dataType   :: a -> DataType

  toDouble   :: a -> Double
  fromDouble :: Double -> a

  gReadByteArray# :: DataType# -> MutableByteArray# s -> Int# -> State# s
                  -> (# State# s, a #)
  gWriteByteArray# :: DataType# -> MutableByteArray# s -> Int# -> a -> State# s
                   -> State# s
  gIndexByteArray# :: DataType# -> ByteArray# -> Int# -> a


{-
convertGType :: forall a b. (GDALType a, GDALType b) => a -> b
convertGType a = runST $ do
  MutableByteArray arr# <- newByteArray (sizeOfDataType dDt)
  primitive_ (gWriteByteArray# dDt# arr# 0# a)
  primitive (gReadByteArray# dDt# arr# 0#)
  where !(I# dDt#) = fromEnum dDt
        dDt        = dataType (undefined :: b)
{-# INLINE convertGType #-}
-}

------------------------------------------------------------------------------
-- DataType
------------------------------------------------------------------------------

type DataType# = Int#

newtype DataType = DataType Int
  deriving (Eq, Show)

instance Enum DataType where
  {-# INLINE fromEnum #-}
  {-# INLINE toEnum #-}
  fromEnum (DataType d) = d
  toEnum                = DataType

gdtByte :: DataType
gdtByte = DataType GDT_BYTE
{-# INLINE gdtByte #-}

gdtUInt16 :: DataType
gdtUInt16 = DataType GDT_UINT16
{-# INLINE gdtUInt16 #-}

gdtUInt32 :: DataType
gdtUInt32 = DataType GDT_UINT32
{-# INLINE gdtUInt32 #-}

gdtInt16 :: DataType
gdtInt16 = DataType GDT_INT16
{-# INLINE gdtInt16 #-}

gdtInt32 :: DataType
gdtInt32 = DataType GDT_INT32
{-# INLINE gdtInt32 #-}

gdtFloat32 :: DataType
gdtFloat32 = DataType GDT_FLOAT32
{-# INLINE gdtFloat32 #-}

gdtFloat64 :: DataType
gdtFloat64 = DataType GDT_FLOAT64
{-# INLINE gdtFloat64 #-}

gdtCInt16 :: DataType
gdtCInt16 = DataType GDT_CINT16
{-# INLINE gdtCInt16 #-}

gdtCInt32 :: DataType
gdtCInt32 = DataType GDT_CINT32
{-# INLINE gdtCInt32 #-}

gdtCFloat32 :: DataType
gdtCFloat32 = DataType GDT_CFLOAT32
{-# INLINE gdtCFloat32 #-}

gdtCFloat64 :: DataType
gdtCFloat64 = DataType GDT_CFLOAT64
{-# INLINE gdtCFloat64 #-}

gdtUnknown :: DataType
gdtUnknown = DataType GDT_UNKNOWN
{-# INLINE gdtUnknown #-}


sizeOfDataType :: DataType -> Int
sizeOfDataType dt
  | dt == gdtByte     = sIZEOF_CHAR
  | dt == gdtUInt16   = sIZEOF_WORD16
  | dt == gdtUInt32   = sIZEOF_WORD32
  | dt == gdtInt16    = sIZEOF_INT16
  | dt == gdtInt32    = sIZEOF_INT32
  | dt == gdtFloat32  = sIZEOF_FLOAT
  | dt == gdtFloat64  = sIZEOF_DOUBLE
  | dt == gdtCInt16   = sIZEOF_INT16 * 2
  | dt == gdtCInt32   = sIZEOF_INT32 * 2
  | dt == gdtCFloat32 = sIZEOF_FLOAT * 2
  | dt == gdtCFloat64 = sIZEOF_DOUBLE * 2
  | otherwise         = error "sizeOfDataType: GDT_Unknown"
{-# INLINE sizeOfDataType #-}



------------------------------------------------------------------------------
-- TMVector
------------------------------------------------------------------------------




data TMVector s a =
  TMVector { mvLen      :: {-# UNPACK #-} !Int
          , mvOff      :: {-# UNPACK #-} !Int
          , mvDataType :: {-# UNPACK #-} !DataType
          , mvData     :: {-# UNPACK #-} !(MutableByteArray s)
          }
  deriving ( Typeable )

instance NFData (TMVector s a) where
  rnf (TMVector _ _ _ _) = ()

newMVector
  :: PrimMonad m
  => DataType
  -> Int
  -> m (TMVector (PrimState m) a)
newMVector dt n
  | n < 0 = error $ "GDAL.TVector.new: negative length: " ++ show n
  | n > mx = error $ "GDAL.TVector.new: length too large: " ++ show n
  | otherwise = do
      arr <- newPinnedByteArray (n*size)
      return TMVector
          { mvLen      = n
          , mvOff      = 0
          , mvDataType = dt
          , mvData     = arr
          }
  where
    mx = maxBound `quot` (sizeOfDataType dt) :: Int
    size = sizeOfDataType dt
{-# INLINE newMVector #-}


type IOTVector = TMVector RealWorld
type STVector s = TMVector s

instance GDALType a => M.MVector TMVector a where
  {-# INLINE basicLength #-}
  basicLength v = mvLen v

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m v = v { mvOff = mvOff v + j
                             , mvLen = m
                             }

  {-# INLINE basicOverlaps #-}
  basicOverlaps TMVector{mvOff=i, mvLen=m, mvData=arr1}
                TMVector{mvOff=j, mvLen=n, mvData=arr2}
    = sameMutableByteArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

  basicUnsafeNew = newMVector (dataType (undefined :: a))
  {-# INLINE basicUnsafeNew #-}

#if MIN_VERSION_vector(0,11,0)
  {-# INLINE basicInitialize #-}
  basicInitialize TMVector{mvOff=off, mvLen=n, mvData=v, mvDataType} =
    setByteArray v (off * size) (n * size) (0 :: Word8)
    where size = sizeOfDataType mvDataType
#endif

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead TMVector{ mvData     = MutableByteArray arr#
                          , mvOff      = I# o#
                          , mvDataType = DataType (I# dt#)
                          } (I# i#) =
    primitive (gReadByteArray# dt# arr# (o# +# i#))


  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite TMVector{ mvData     = MutableByteArray arr#
                           , mvOff      = I# o#
                           , mvDataType = DataType (I# dt#)
                           } (I# i#) v =
    primitive_ (gWriteByteArray# dt# arr# (o# +# i#) v)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dVec@TMVector{mvDataType=dDt, mvOff=i, mvLen=n, mvData=dst}
                  sVec@TMVector{mvDataType=sDt, mvOff=j, mvData=src}
    | sDt == dDt = copyMutableByteArray dst (i*sz) src (j*sz) (n*sz)
    | otherwise  = loop 0
    where
      sz = sizeOfDataType dDt
      loop !ix
        | ix < mvLen dVec = do
            M.basicUnsafeRead sVec ix >>= M.basicUnsafeWrite dVec ix
            loop (ix+1)
        | otherwise      = return ()

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove dVec@TMVector{mvDataType=dDt, mvOff=i, mvLen=n, mvData=dst}
                  sVec@TMVector{mvDataType=sDt, mvOff=j, mvData=src}
    | sDt == dDt = moveByteArray dst (i*sz) src (j*sz) (n*sz)
    | otherwise  = M.basicUnsafeCopy dVec sVec
    where
      sz = sizeOfDataType dDt


------------------------------------------------------------------------------
-- TVector
------------------------------------------------------------------------------

data TVector a =
  TVector { vLen     :: {-# UNPACK #-} !Int
          , vOff      :: {-# UNPACK #-} !Int
          , vDataType :: {-# UNPACK #-} !DataType
          , vData     :: {-# UNPACK #-} !ByteArray
          }
  deriving ( Typeable )

type instance G.Mutable TVector = TMVector

castTVector :: (GDALType a, GDALType b) => TVector a -> TVector b
castTVector = unsafeCoerce
{-# INLINE castTVector #-}

instance GDALType a => G.Vector TVector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze mv = do
    arr <- unsafeFreezeByteArray (mvData mv)
    return $ TVector { vLen      = mvLen mv
                    , vOff      = mvOff mv
                    , vDataType = mvDataType mv
                    , vData     = arr
                    }

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw v = do
    arr <- unsafeThawByteArray (vData v)
    return $ TMVector { mvLen      = vLen v
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
  basicUnsafeIndexM TVector{ vData     = ByteArray arr#
                           , vOff      = I# o#
                           , vDataType = DataType (I# dt#)
                           } (I# i#) =
    return $! gIndexByteArray# dt# arr# (o# +# i#)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy dVec@TMVector{mvDataType=dDt, mvOff=i, mvLen=n, mvData=dst}
                  sVec@TVector{vDataType=sDt, vOff=j, vData=src}
    | sDt == dDt = copyByteArray dst (i*sz) src (j*sz) (n*sz)
    | otherwise  = loop 0
    where
      sz = sizeOfDataType dDt
      loop !ix
        | ix < mvLen dVec = do
            G.basicUnsafeIndexM sVec ix >>= M.basicUnsafeWrite dVec ix
            loop (ix+1)
        | otherwise      = return ()

  {-# INLINE elemseq #-}
  elemseq _ = seq

instance NFData (TVector a) where
  rnf (TVector _ _ _ _) = ()



unsafeAsNative :: TVector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeAsNative TVector{vData, vDataType, vOff} f = do
  r <- f vDataType (Ptr addr)
  touch vData
  return r
  where !(Addr addr) = byteArrayContents vData `plusAddr` (vOff*size)
        size = sizeOfDataType vDataType
{-# INLINE unsafeAsNative #-}

unsafeAsNativeM
  :: IOTVector a -> (DataType -> Ptr () -> IO b) -> IO b
unsafeAsNativeM TMVector{mvData, mvOff, mvDataType} f = do
  r <- f mvDataType (Ptr addr)
  touch mvData
  return r
  where !(Addr addr) = mutableByteArrayContents mvData `plusAddr` (mvOff*size)
        size = sizeOfDataType mvDataType
{-# INLINE unsafeAsNativeM #-}

unsafeAsDataType
  :: GDALType a => DataType -> TVector a -> (Ptr () -> IO b) -> IO b
unsafeAsDataType dt v f
  | dt == vDataType v = unsafeAsNative v (const f)
  | otherwise = do
      copy <- newMVector dt (vLen v)
      G.basicUnsafeCopy copy v
      unsafeAsNativeM copy (const f)
{-# INLINE unsafeAsDataType #-}


------------------------------------------------------------------------------
-- MaskedVector
------------------------------------------------------------------------------


maskValid, maskNoData :: Word8
maskValid  = 255
maskNoData = 0

data Mask v
  = AllValid
  | Mask      (v Word8)
  | UseNoData Double

instance (GDALType a) => U.Unbox (Value a)

newtype instance U.Vector (Value a) =
  MaskedVector (Mask St.Vector, TVector a)
  deriving Typeable

newWithMask
  :: St.Vector Word8 -> TVector a -> U.Vector (Value a)
newWithMask mask values = MaskedVector (Mask mask, values)

newAllValid :: TVector a -> U.Vector (Value a)
newAllValid values = MaskedVector (AllValid, values)

newWithNoData
  :: (GDALType a) => a -> TVector a -> U.Vector (Value a)
newWithNoData a values = MaskedVector (UseNoData (toDouble a), values)

newtype instance U.MVector s (Value a) =
  MaskedMVector (Mask (St.MVector s), TMVector s a)
  deriving Typeable

newWithMaskM
  :: St.MVector s Word8
  -> TMVector s a
  -> U.MVector s (Value a)
newWithMaskM mask values = MaskedMVector (Mask mask, values)
{-# INLINE newWithMaskM #-}

newAllValidM
  :: TMVector s a -> U.MVector s (Value a)
newAllValidM values = MaskedMVector (AllValid, values)
{-# INLINE newAllValidM #-}

newWithNoDataM
  :: (GDALType a)
  => a -> TMVector s a -> U.MVector s (Value a)
newWithNoDataM a values = MaskedMVector (UseNoData (toDouble a), values)

{-# INLINE newWithNoDataM #-}


toBaseVectorWithNoData
  :: (GDALType a) => a -> U.Vector (Value a) -> TVector a
toBaseVectorWithNoData nd v =
   (G.generate (G.length v) (fromValue nd . G.unsafeIndex v))

toBaseVectorWithMask
  :: (GDALType a, GDALType Double)
  => U.Vector (Value a) -> (St.Vector Word8, TVector a)
toBaseVectorWithMask (MaskedVector (Mask m  , vs)) = (m, vs)
toBaseVectorWithMask (MaskedVector (AllValid, vs)) =
  ((G.replicate (G.length vs) maskValid), vs)
toBaseVectorWithMask (MaskedVector (UseNoData nd, vs)) =
  (G.generate (G.length vs)
   (\i-> if castTVector vs `G.unsafeIndex` i == nd
            then maskNoData
            else maskValid), vs)

toBaseVector
  :: (GDALType a, GDALType Double)
  => U.Vector (Value a) -> Maybe (TVector a)
toBaseVector (MaskedVector (AllValid, v)) = Just v
toBaseVector (MaskedVector (UseNoData nd, v))
  | G.any (==nd) (castTVector v) = Nothing
  | otherwise                       = Just v
toBaseVector (MaskedVector (Mask m, v))
  | G.any (==maskNoData) m = Nothing
  | otherwise              = Just  v



instance (GDALType a, G.Vector TVector a)
  => G.Vector U.Vector (Value a) where
  basicUnsafeFreeze (MaskedMVector (Mask x,v)) =
    liftM2 (\x' v' -> MaskedVector (Mask x',v'))
           (G.basicUnsafeFreeze x)
           (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MaskedMVector (AllValid,v)) =
    liftM (\v' -> MaskedVector (AllValid,v')) (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MaskedMVector (UseNoData nd,v)) =
    liftM (\v' -> MaskedVector (UseNoData nd,v')) (G.basicUnsafeFreeze v)
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (MaskedVector (Mask x,v)) =
    liftM2 (\x' v' -> MaskedMVector (Mask x',v'))
           (G.basicUnsafeThaw x)
           (G.basicUnsafeThaw v)
  basicUnsafeThaw (MaskedVector (AllValid,v)) =
    liftM (\v' -> MaskedMVector (AllValid,v')) (G.basicUnsafeThaw v)
  basicUnsafeThaw (MaskedVector (UseNoData nd,v)) =
    liftM (\v' -> MaskedMVector (UseNoData nd,v')) (G.basicUnsafeThaw v)
  {-# INLINE basicUnsafeThaw #-}

  basicLength  (MaskedVector (_,v)) = G.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MaskedVector (Mask x,v)) =
    MaskedVector (Mask (G.basicUnsafeSlice m n x), G.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (MaskedVector (x,v)) =
    MaskedVector (x, G.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (MaskedVector (Mask x,v)) i = do
    m <- G.basicUnsafeIndexM x i
    if m/=maskNoData
       then liftM Value (G.basicUnsafeIndexM v i)
       else return NoData
  basicUnsafeIndexM (MaskedVector (AllValid,v)) i =
     liftM Value (G.basicUnsafeIndexM v i)
  basicUnsafeIndexM (MaskedVector (UseNoData nd,v)) i = do
    val <- G.basicUnsafeIndexM v i
    return (if toDouble val==nd then NoData else Value val)
  {-# INLINE basicUnsafeIndexM #-}



instance (GDALType a, M.MVector TMVector a) =>
  M.MVector U.MVector (Value a) where
  basicLength (MaskedMVector (_,v)) = M.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MaskedMVector (Mask x,v)) =
    MaskedMVector ( Mask (M.basicUnsafeSlice m n x)
             , M.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (MaskedMVector (x,v)) =
    MaskedMVector (x, M.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (MaskedMVector (_,v)) (MaskedMVector (_,v')) = M.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew i =
    liftM2 newWithMaskM (M.basicUnsafeNew i) (M.basicUnsafeNew i)
  {-# INLINE basicUnsafeNew #-}


  basicUnsafeRead (MaskedMVector (Mask x,v)) i = do
    m <- M.basicUnsafeRead x i
    if m/=maskNoData
       then liftM Value (inline M.basicUnsafeRead v i)
       else return NoData
  basicUnsafeRead (MaskedMVector (AllValid,v)) i =
    liftM Value (inline M.basicUnsafeRead v i)
  basicUnsafeRead (MaskedMVector (UseNoData nd,v)) i = do
    val <- inline M.basicUnsafeRead v i
    return (if toDouble val == nd then NoData else Value val)
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (MaskedMVector (Mask x,v)) i a
    | isNoData a  = M.basicUnsafeWrite x i maskNoData
    | otherwise = do
        M.basicUnsafeWrite x i maskValid
        M.basicUnsafeWrite v i (fromValue unexpectedNull a)
  basicUnsafeWrite (MaskedMVector (AllValid,v)) i a
    | isNoData a  = error ("GDAL.MaskedVector.Masked.basicUnsafeWrite: " ++
                         "tried to write a nullValue in an AllValid vector")
    | otherwise = M.basicUnsafeWrite v i (fromValue unexpectedNull a)
  basicUnsafeWrite (MaskedMVector (UseNoData nd,v)) i a =
    M.basicUnsafeWrite v i (fromValue (fromDouble nd) a)
  {-# INLINE basicUnsafeWrite #-}

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MaskedMVector (Mask x,v)) = do
    M.basicInitialize x
    M.basicInitialize v
  basicInitialize (MaskedMVector (_,v)) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

unexpectedNull :: t
unexpectedNull =
  error "GDAL.MaskedVector.Masked: Unexpected null element from fromValue"
