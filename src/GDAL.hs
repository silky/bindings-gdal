module GDAL (
    GDAL
  , GDALType
  , ApproxOK (..)
  , DataType
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
  , XY (..)
  , Envelope (..)
  , Size
  , BlockIx

  , GDALException (..)
  , GDALRasterException (..)
  , ProgressException (..)
  , ErrorType (..)
  , ErrorNum (..)
  , isGDALException
  , isBindingException
  , isProgressFunException
  , isInterruptedException

  , Geotransform (..)
  , GroundControlPoint (..)
  , OverviewResampling (..)
  , Driver (..)
  , Dataset
  , OptionList
  , ReadWrite
  , ReadOnly
  , RWDataset
  , RODataset
  , MaskType (MaskPerBand, MaskPerDataset)
  , Value (..)
  , ProgressFun
  , ContinueOrStop (..)

  , dataType
  , runGDAL
  , execGDAL
  , withGDAL
  , isNoData
  , fromValue
  , create
  , createMem
  , delete
  , rename
  , copyFiles
  , flushCache
  , openReadOnly
  , openReadWrite
  , closeDataset
  , unsafeToReadOnly
  , createCopy
  , buildOverviews

  , datasetDriver
  , datasetSize
  , datasetFileList
  , datasetProjection
  , setDatasetProjection
  , datasetGeotransform
  , setDatasetGeotransform
  , datasetGCPs
  , setDatasetGCPs

  , sizeLen

  , metadataDomains
  , metadata
  , metadataItem
  , setMetadataItem
  , description
  , setDescription

  , gcp

  , version

  , gcpGeotransform
  , northUpGeotransform
  , applyGeotransform
  , (|$|)
  , invertGeotransform
  , inv
  , composeGeotransforms
  , (|.|)

  , liftIO
) where

import Control.Exception (finally)
import Control.Monad.IO.Class (liftIO)

import GDAL.Band.Generic

import GDAL.Internal.CPLError
import GDAL.Internal.DataType
import GDAL.Internal.DataType.Instances()
import GDAL.Internal.GDAL.Types
import GDAL.Internal.CPLString
import GDAL.Internal.CPLProgress
import GDAL.Internal.GCP
import GDAL.Internal.GDAL as GDAL
import GDAL.Internal.Types
import GDAL.Internal.Types.Value
import GDAL.Internal.Common

import qualified GDAL.Internal.OGR as OGR
import qualified GDAL.Internal.OSR as OSR

-- | Performs process-wide initialization and cleanup
--   Should only be called from the main thread
withGDAL :: IO a -> IO a
withGDAL a =
    (GDAL.allRegister >> OGR.registerAll >> OSR.initialize >> a)
      `finally` (OGR.cleanupAll >> GDAL.destroyDriverManager >> OSR.cleanup)
