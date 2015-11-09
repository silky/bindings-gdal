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

module GDAL.Internal.DataType (
    GDALType (..)
  , DataType (..)
  , MVector
  , Vector
  , IOVector
  , STVector
  , sizeOfDataType
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


data DataType
  = GDT_Unknown
  | GDT_Byte
  | GDT_UInt16
  | GDT_Int16
  | GDT_UInt32
  | GDT_Int32
  | GDT_Float32
  | GDT_Float64
  | GDT_CInt16
  | GDT_CInt32
  | GDT_CFloat32
  | GDT_CFloat64
  deriving (Eq, Show, Enum, Bounded)

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
