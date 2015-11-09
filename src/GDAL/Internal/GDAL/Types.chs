{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.GDAL.Types (
    BandH (..)
  , DataType (..)
  , Dataset (..)
  , DatasetH (..)
  , Driver (..)
  , DriverH
  , GDALAccess (..)
  , GDALRasterException (..)
  , MaskType (..)
  , OptionList
  , ProgressFun

  , MajorObject (..)
  , MajorObjectH (..)

  , RODataset
  , RWDataset

  , RWFlag (..)

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

  , maskFlagsForType

  , nullDatasetH
  , nullBandH


  , unDataset
) where

#include "gdal.h"
{#context lib = "gdal" prefix = "GDAL" #}


import GDAL.Internal.Types
import GDAL.Internal.DataType (DataType(..))
import GDAL.Internal.CPLError (
    bindingExceptionToException
  , bindingExceptionFromException
  )
import GDAL.Internal.CPLProgress (ProgressFun)
import GDAL.Internal.CPLString (OptionList)

import Control.Exception (Exception(..))

import Data.ByteString.Char8 (ByteString)
import Data.String (IsString)
import Data.Typeable (Typeable)

import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)

import System.IO.Unsafe (unsafePerformIO)



{#enum GDALAccess {} deriving (Eq, Show) #}

{#enum RWFlag {} deriving (Eq, Show) #}


------------------------------------------------------------------------------
-- GDALRasterException
------------------------------------------------------------------------------

data GDALRasterException
  = InvalidRasterSize !Size
  | InvalidBlockSize  !Int
  | InvalidDriverOptions
  | UnknownRasterDataType
  | UnsupportedRasterDataType !DataType
  | NullDataset
  | NullBand
  | UnknownDriver !ByteString
  | BandDoesNotAllowNoData
  | NonNativeDataType !DataType
  deriving (Typeable, Show, Eq)

instance Exception GDALRasterException where
  toException   = bindingExceptionToException
  fromException = bindingExceptionFromException

------------------------------------------------------------------------------
-- MajorObject
------------------------------------------------------------------------------

{#pointer MajorObjectH newtype#}

class MajorObject o (t::AccessMode) where
  majorObject     :: o t -> MajorObjectH


------------------------------------------------------------------------------
-- Dataset
------------------------------------------------------------------------------

{#pointer DatasetH newtype #}


nullDatasetH :: DatasetH
nullDatasetH = DatasetH nullPtr

deriving instance Eq DatasetH

newtype Dataset s (t::AccessMode) =
  Dataset (ReleaseKey, DatasetH)

instance MajorObject (Dataset s) t where
  majorObject ds =
    let DatasetH p = unDataset ds
    in MajorObjectH (castPtr p)

unDataset :: Dataset s t -> DatasetH
unDataset (Dataset (_,s)) = s


type RODataset s = Dataset s ReadOnly
type RWDataset s = Dataset s ReadWrite

------------------------------------------------------------------------------
-- RasterBand
------------------------------------------------------------------------------


{#pointer RasterBandH as BandH newtype #}

nullBandH :: BandH
nullBandH = BandH nullPtr

deriving instance Eq BandH



------------------------------------------------------------------------------
-- Driver
------------------------------------------------------------------------------

{#pointer DriverH #}

newtype Driver = Driver ByteString
  deriving (Eq, IsString)

instance Show Driver where
  show (Driver s) = show s


------------------------------------------------------------------------------
-- DataType
------------------------------------------------------------------------------

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


data MaskType
  = MaskNoData
  | MaskAllValid
  | MaskPerBand
  | MaskPerDataset
  deriving Show

maskFlagsForType :: MaskType -> CInt
maskFlagsForType MaskNoData      = {#const GMF_NODATA#}
maskFlagsForType MaskAllValid    = {#const GMF_ALL_VALID#}
maskFlagsForType MaskPerBand     = 0
maskFlagsForType MaskPerDataset  = {#const GMF_PER_DATASET#}
{-# INLINE maskFlagsForType #-}
