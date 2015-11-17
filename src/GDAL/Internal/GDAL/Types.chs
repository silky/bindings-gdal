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
