{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import System.Environment (getArgs)
import Control.Monad

import qualified GDAL.Band.Masked.Translated2 as B
import GDAL

type SummaryType = Double
minSumBound, maxSumBound :: SummaryType
maxSumBound = 1/0
minSumBound = (-1/0)
convert :: Double -> SummaryType
convert2 :: SummaryType -> Double
convert = id
convert2 = id

main :: IO ()
main = withGDAL $ do
  [fname] <- getArgs
  (avg,stddev,aMin,aMax) <- execGDAL $ do
    b <- openReadOnly fname >>= B.getBand 1
    computeStatistics convert convert2 b
  putStrLn ("average: " ++ show avg)
  putStrLn ("stddev: " ++ show stddev)
  putStrLn ("min: " ++ show aMin)
  putStrLn ("max: " ++ show aMax)

data Acc = Acc
  { accS   :: {-# UNPACK #-} !SummaryType
  , accSq  :: {-# UNPACK #-} !SummaryType
  , accMin :: {-# UNPACK #-} !SummaryType
  , accMax :: {-# UNPACK #-} !SummaryType
  , accCnt :: {-# UNPACK #-} !Int
  }

type Summary = (Double, Double, SummaryType, SummaryType)

computeStatistics
  :: GDALType a
  => (a -> SummaryType)
  -> (SummaryType -> Double)
  -> B.Band s (Value a) t
  -> GDAL s Summary
computeStatistics f f2
  = fmap sumarize . B.foldl' folder (Acc 0 0 maxSumBound minSumBound 0)
  where
    folder acc NoData = acc
    folder Acc{..} (Value v')
      = Acc (accS+v) (accSq+v*v) (min accMin v) (max accMax v) (accCnt+1)
        where v = f v'
    sumarize Acc{..} = (avg, stddev, accMin, accMax)
      where
        avg    = f2 accS  / fromIntegral accCnt
        stddev = sqrt (f2 accSq / fromIntegral accCnt - avg*avg)
{-# INLINE computeStatistics #-}
