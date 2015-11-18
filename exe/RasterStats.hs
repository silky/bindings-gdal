{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import System.Environment (getArgs)
import Data.Int (Int64, Int16)
import Control.Monad

import qualified GDAL.Band.Masked.Translated as B
import GHC.Exts (inline)
import GDAL

type SummaryType = Int
maxSumBound = maxBound -- 1/0
minSumBound = minBound -- (-1/0)
convert :: Int16 -> SummaryType
convert2 :: SummaryType -> Double
convert = fromIntegral
convert2 = fromIntegral

main :: IO ()
main = withGDAL $ do
  [fname] <- getArgs
  summary <- execGDAL $ do
    b <- openReadOnly fname >>= B.getBand 1
    when (not (B.isNative b)) $ error "caca"
    computeStatistics convert convert2 b
  print summary

data Acc = Acc
  { accS   :: {-# UNPACK #-} !SummaryType
  , accSq  :: {-# UNPACK #-} !SummaryType
  , accMin :: {-# UNPACK #-} !SummaryType
  , accMax :: {-# UNPACK #-} !SummaryType
  , accCnt :: {-# UNPACK #-} !Int
  }

type Summary = (Double, Double, SummaryType, SummaryType)

computeStatistics f f2
  = fmap sumarize . inline B.foldl' folder (Acc 0 0 maxSumBound minSumBound 0)
  where
    folder acc NoData = acc
    folder Acc{..} (Value v')
      = Acc (accS+v) (accSq+v*v) (min accMin v) (max accMax v) (accCnt+1)
        where v = f v'
    sumarize Acc{..} = (avg, stddev, accMin, accMax)
      where
        avg    = f2 accS  / fromIntegral accCnt
        stddev = sqrt (f2 accSq / fromIntegral accCnt - avg*avg)
