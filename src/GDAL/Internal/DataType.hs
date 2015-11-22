{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module GDAL.Internal.DataType (
    DataType (..)
  , GDALType (..)
  , DataTypeK (..)
  , HsType
  , IsComplex
  , GDT_Byte
  , GDT_UInt16
  , GDT_UInt32
  , GDT_Int16
  , GDT_Int32
  , GDT_Float32
  , GDT_Float64
  , GDT_CInt16
  , GDT_CInt32
  , GDT_CFloat32
  , GDT_CFloat64

  , sizeOfDataType
) where

import GDAL.Internal.Types (Pair(..), pFst)
import GDAL.Internal.DataType.Internal
import Data.Word
import Data.Int

import Foreign.Storable (Storable(sizeOf))
import Foreign.C.Types (CDouble(..))


data DataType :: DataTypeK -> * where
  GDT_Byte     :: DataType GDT_Byte
  GDT_UInt16   :: DataType GDT_UInt16
  GDT_UInt32   :: DataType GDT_UInt32
  GDT_Int16    :: DataType GDT_Int16
  GDT_Int32    :: DataType GDT_Int32
  GDT_Float32  :: DataType GDT_Float32
  GDT_Float64  :: DataType GDT_Float64
  GDT_CInt16   :: DataType GDT_CInt16
  GDT_CInt32   :: DataType GDT_CInt32
  GDT_CFloat32 :: DataType GDT_CFloat32
  GDT_CFloat64 :: DataType GDT_CFloat64

instance Show (DataType a) where
  show = show . dataTypeK

instance Enum (DataType a) where
  fromEnum = fromEnum . dataTypeK
  toEnum   = error "toEnum (DataType a) is not implemented"

dataTypeK :: DataType a -> DataTypeK
dataTypeK GDT_Byte                   = GByte
dataTypeK GDT_UInt16                 = GUInt16
dataTypeK GDT_UInt32                 = GUInt32
dataTypeK GDT_Int16                  = GInt16
dataTypeK GDT_Int32                  = GInt32
dataTypeK GDT_Float32                = GFloat32
dataTypeK GDT_Float64                = GFloat64
dataTypeK GDT_CInt16                 = GCInt16
dataTypeK GDT_CInt32                 = GCInt32
dataTypeK GDT_CFloat32               = GCFloat32
dataTypeK GDT_CFloat64               = GCFloat64

reifyDataTypeK
  :: DataTypeK
  -> (forall d. GDALType (HsType d) => DataType d -> b)
  -> b
reifyDataTypeK GByte     f = f GDT_Byte
reifyDataTypeK GUInt16   f = f GDT_UInt16
reifyDataTypeK GUInt32   f = f GDT_UInt32
reifyDataTypeK GInt16    f = f GDT_Int16
reifyDataTypeK GInt32    f = f GDT_Int32
reifyDataTypeK GFloat32  f = f GDT_Float32
reifyDataTypeK GFloat64  f = f GDT_Float64
reifyDataTypeK GCInt16   f = f GDT_CInt16
reifyDataTypeK GCInt32   f = f GDT_CInt32
reifyDataTypeK GCFloat32 f = f GDT_CFloat32
reifyDataTypeK GCFloat64 f = f GDT_CFloat64
reifyDataTypeK GUnknown  _ = error "GDAL.DataType.reifyDataTypeK: GDT_Unknown"

class ( Storable a
      , Eq a
      , Show a
      , Num a
      -- This constraint ensures only one instance is defined per HsType
      -- in a sort-of poor's man injective type function. We need it to
      -- be sure that reifyDataTypeK makes sense
      , HsType (TypeK a) ~ a
      ) => GDALType a where
  type TypeK a  :: DataTypeK
  dataType      :: a -> DataTypeK
  toCDouble     :: a -> CDouble
  fromCDouble   :: CDouble -> a


type family HsType (a :: DataTypeK) where
  HsType GDT_Byte     = Word8
  HsType GDT_UInt16   = Word16
  HsType GDT_UInt32   = Word32
  HsType GDT_Int16    = Int16
  HsType GDT_Int32    = Int32
  HsType GDT_Float32  = Float
  HsType GDT_Float64  = Double
  HsType GDT_CInt16   = Pair Int16
  HsType GDT_CInt32   = Pair Int32
  HsType GDT_CFloat32 = Pair Float
  HsType GDT_CFloat64 = Pair Double

type family IsComplex (a :: DataTypeK) where
  IsComplex GDT_Byte     = 'False
  IsComplex GDT_UInt16   = 'False
  IsComplex GDT_UInt32   = 'False
  IsComplex GDT_Int16    = 'False
  IsComplex GDT_Int32    = 'False
  IsComplex GDT_Float32  = 'False
  IsComplex GDT_Float64  = 'False
  IsComplex GDT_CInt16   = 'True
  IsComplex GDT_CInt32   = 'True
  IsComplex GDT_CFloat32 = 'True
  IsComplex GDT_CFloat64 = 'True

sizeOfDataType :: DataTypeK -> Int
sizeOfDataType dt = reifyDataTypeK dt (sizeOf . hsType)
{-# INLINE sizeOfDataType #-}

hsType :: Storable (HsType d) => DataType d -> HsType d
hsType = const undefined

------------------------------------------------------------------------------
-- GDALType
------------------------------------------------------------------------------

instance GDALType Word8 where
  type TypeK Word8 = GDT_Byte
  dataType _       = GByte
  toCDouble        = fromIntegral
  fromCDouble      = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word16 where
  type TypeK Word16 = GDT_UInt16
  dataType _        = GUInt16
  toCDouble         = fromIntegral
  fromCDouble       = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Word32 where
  type TypeK Word32 = GDT_UInt32
  dataType _        = GUInt32
  toCDouble         = fromIntegral
  fromCDouble       = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int16 where
  type TypeK Int16 = GDT_Int16
  dataType _       = GInt16
  toCDouble        = fromIntegral
  fromCDouble      = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Int32 where
  type TypeK Int32 = GDT_Int32
  dataType _       = GInt32
  toCDouble        = fromIntegral
  fromCDouble      = truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Float where
  type TypeK Float = GDT_Float32
  dataType _       = GFloat32
  toCDouble        = realToFrac
  fromCDouble      = realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType Double where
  type TypeK Double = GDT_Float64
  dataType _        = GFloat64
  toCDouble         = realToFrac
  fromCDouble       = realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Int16) where
  type TypeK (Pair Int16) = GDT_CInt16
  dataType _              = GCInt16
  toCDouble               = fromIntegral . pFst
  fromCDouble             = (:+: 0) . truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Int32) where
  type TypeK (Pair Int32) = GDT_CInt32
  dataType _              = GCInt32
  toCDouble               = fromIntegral . pFst
  fromCDouble             = (:+: 0) . truncate
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Float) where
  type TypeK (Pair Float) = GDT_CFloat32
  dataType _              = GCFloat32
  toCDouble               = realToFrac . pFst
  fromCDouble             = (:+: 0) . realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}

instance GDALType (Pair Double) where
  type TypeK (Pair Double) = GDT_CFloat64
  dataType _               = GCFloat64
  toCDouble                = realToFrac . pFst
  fromCDouble              = (:+: 0) . realToFrac
  {-# INLINE dataType #-}
  {-# INLINE toCDouble #-}
  {-# INLINE fromCDouble #-}