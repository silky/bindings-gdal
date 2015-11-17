{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

#include "bindings.h"

module GDAL.Internal.DataType (
    GDALType (..)
  , DataType (..)
  , MVector
  , Vector
  , IOVector
  , STVector

  , sizeOfDataType
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


import Control.Monad.Primitive

import Data.Int (Int8, Int16, Int32)
import Data.Primitive.Types
import Data.Primitive.MachDeps
import Data.Word (Word8, Word16, Word32)

import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as M

import Foreign.Ptr (Ptr)


data family Vector    a
data family MVector s a
type instance G.Mutable Vector = MVector

type STVector = MVector
type IOVector = MVector RealWorld


------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------
class (G.Vector Vector a , M.MVector MVector a) => GDALType a where
  dataType        :: a -> DataType
  newMVector      :: PrimMonad m
                  => DataType -> Int -> m (MVector (PrimState m) a)
  unsafeAsNative  :: Vector a -> (DataType -> Ptr () -> IO b) -> IO b
  unsafeAsDataType  :: DataType -> Vector a -> (Ptr () -> IO b) -> IO b
  unsafeAsNativeM :: IOVector a -> (DataType -> Ptr () -> IO b) -> IO b

  gFromIntegral :: Integral b => b -> a
  gToIntegral   :: Integral b => a -> b
  gFromReal :: RealFrac b => b -> a
  gToReal   :: RealFrac b => a -> b

  gFromIntegralPair :: Integral b => Pair b -> a
  gToIntegralPair   :: Integral b => a -> Pair b
  gFromRealPair :: RealFrac b => Pair b -> a
  gToRealPair   :: RealFrac b => a -> Pair b


------------------------------------------------------------------------------
-- DataType
------------------------------------------------------------------------------


data DataType
  = GDT_Unknown
  | GDT_Byte
  | GDT_UInt16
  | GDT_UInt32
  | GDT_Int16
  | GDT_Int32
  | GDT_Float32
  | GDT_Float64
  | GDT_CInt16
  | GDT_CInt32
  | GDT_CFloat32
  | GDT_CFloat64
  deriving (Eq, Show)

instance Enum DataType where
  {-# INLINE fromEnum #-}
  {-# INLINE toEnum #-}
  fromEnum dt =
    case dt of
      GDT_Unknown  -> GDT_UNKNOWN
      GDT_Byte     -> GDT_BYTE
      GDT_UInt16   -> GDT_UINT16
      GDT_UInt32   -> GDT_UINT32
      GDT_Int16    -> GDT_INT16
      GDT_Int32    -> GDT_INT32
      GDT_Float32  -> GDT_FLOAT32
      GDT_Float64  -> GDT_FLOAT64
      GDT_CInt16   -> GDT_CINT16
      GDT_CInt32   -> GDT_CINT32
      GDT_CFloat32 -> GDT_CFLOAT32
      GDT_CFloat64 -> GDT_CFLOAT64
  toEnum i =
    case i of
      GDT_UNKNOWN  -> GDT_Unknown
      GDT_BYTE     -> GDT_Byte
      GDT_UINT16   -> GDT_UInt16
      GDT_UINT32   -> GDT_UInt32
      GDT_INT16    -> GDT_Int16
      GDT_INT32    -> GDT_Int32
      GDT_FLOAT32  -> GDT_Float32
      GDT_FLOAT64  -> GDT_Float64
      GDT_CINT16   -> GDT_CInt16
      GDT_CINT32   -> GDT_CInt32
      GDT_CFLOAT32 -> GDT_CFloat32
      GDT_CFLOAT64 -> GDT_CFloat64
      _            -> error "toEnum DataType: Invalid value"

gdtByte :: DataType
gdtByte = GDT_Byte

gdtUInt16 :: DataType
gdtUInt16 = GDT_UInt16

gdtUInt32 :: DataType
gdtUInt32 = GDT_UInt32

gdtInt16 :: DataType
gdtInt16 = GDT_Int16

gdtInt32 :: DataType
gdtInt32 = GDT_Int32

gdtFloat32 :: DataType
gdtFloat32 = GDT_Float32

gdtFloat64 :: DataType
gdtFloat64 = GDT_Float64

gdtCInt16 :: DataType
gdtCInt16 = GDT_CInt16

gdtCInt32 :: DataType
gdtCInt32 = GDT_CInt32

gdtCFloat32 :: DataType
gdtCFloat32 = GDT_CFloat32

gdtCFloat64 :: DataType
gdtCFloat64 = GDT_CFloat64

gdtUnknown :: DataType
gdtUnknown = GDT_Unknown


sizeOfDataType :: DataType -> Int
sizeOfDataType dt
  | dt == GDT_Byte     = sIZEOF_CHAR
  | dt == GDT_UInt16   = sIZEOF_WORD16
  | dt == GDT_UInt32   = sIZEOF_WORD32
  | dt == GDT_Int16    = sIZEOF_INT16
  | dt == GDT_Int32    = sIZEOF_INT32
  | dt == GDT_Float32  = sIZEOF_FLOAT
  | dt == GDT_Float64  = sIZEOF_DOUBLE
  | dt == GDT_CInt16   = sIZEOF_INT16 * 2
  | dt == GDT_CInt32   = sIZEOF_INT32 * 2
  | dt == GDT_CFloat32 = sIZEOF_FLOAT * 2
  | dt == GDT_CFloat64 = sIZEOF_DOUBLE * 2
  | otherwise         = error "sizeOfDataType: GDT_Unknown"
{-# INLINE sizeOfDataType #-}
