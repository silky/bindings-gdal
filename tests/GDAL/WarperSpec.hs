{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module GDAL.WarperSpec (main, spec) where

import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as U
import Lens.Micro

import GDAL
import OSR
import OGR (geomFromWkt)
import GDAL.Warper
import GDAL.Algorithms

import TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = setupAndTeardown $ do

  describe "reprojectImage" $ do

    it "works with SpatialReferences in dataset" $ do
      let Right srs1 = srsFromEPSG 23030
          Right srs2 = srsFromEPSG 4326

      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetProjection srs1 ds'
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds'
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetProjection srs2 ds2
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds2
      reprojectImage ds ds2 def

    it "does not work with no geotransforms" $ do
      let Right srs1 = srsFromEPSG 23030
          Right srs2 = srsFromEPSG 4326
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetProjection srs1 ds'
      ds <- unsafeToReadOnly ds'
      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetProjection srs2 ds2
      reprojectImage ds ds2 def `shouldThrow` ((==AppDefined) . gdalErrNum)

    it "works with SpatialReferences as args" $ do
      let Right srs1 = srsFromEPSG 23030
          Right srs2 = srsFromEPSG 4326
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds'
      ds <- unsafeToReadOnly ds'
      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds2
      reprojectImage ds ds2 $ def
        & srcSrs .~ Just srs1
        & dstSrs .~ Just srs2

    it "can be stopped with progressFun" $ do
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds'
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds2
      let opts = def & progressFun .~ Just (\_ _ -> return Stop)
      reprojectImage ds ds2 opts `shouldThrow` isInterruptedException

    it "can receive warp options" $ do
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds'
      ds <- unsafeToReadOnly ds'

      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds2
      reprojectImage ds ds2 $ def
        & options .~ [("OPTIMIZE_SIZE","TRUE")]

    it "can receive cutline" $ do
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds'
      ds <- unsafeToReadOnly ds'

      let Right cl = geomFromWkt Nothing
                     "POLYGON ((0 0, 0 100, 100 100, 100 0, 0 0))"

      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds2
      reprojectImage ds ds2 $ def
        & cutline .~ Just cl

    it "cutline must be a polygon" $ do
      ds' <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds'
      ds <- unsafeToReadOnly ds'

      let Right cl = geomFromWkt Nothing "POINT (0 0)"
      ds2 <- createMem (100 :+: 100) 1 GDT_Int16 []
      setDatasetGeotransform (Geotransform 0 10 0 0 0 (-10)) ds2
      let opts = def & cutline .~ Just cl
      reprojectImage ds ds2 opts `shouldThrow` (==NonPolygonCutline)

    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata " ++ show algo) $ do
        let sz  = 100 :+: 100
            sz2 = 200 :+: 200
            gt  = Geotransform 0 10 0 0 0 (-10)
            v1  = U.generate (sizeLen sz)
                  (\i -> if i<50 then NoData else Value (fromIntegral i))
        ds' <- createMem sz 1 GDT_Int32 []
        setDatasetGeotransform gt ds'
        b <- getBand 1 ds'
        setBandNodataValue (-1) b
        writeBand b (allBand b) sz v1
        ds <- unsafeToReadOnly ds'

        ds2 <- createMem sz2 1 GDT_Int32 []
        setDatasetGeotransform gt ds2
        b2 <- getBand 1 ds2
        setBandNodataValue (-2) b2

        reprojectImage ds ds2 $ def & resampleAlg .~ algo
        flushCache ds2

        v2 <- readBand b2 (allBand b2) sz2
        catValues v2 `shouldSatisfy` U.all (> 0)
        U.sum (catValues v2) `shouldBe` (U.sum (catValues v1))

  describe "createWarpedVRT" $ do

{-
    it "can receive cutline" $ do
      let gt = northUpGeotransform 100 (Envelope (-500) 500)
          Right cl = geomFromWkt Nothing
                     "POLYGON ((0 0, 0 100, 100 100, 100 0, 0 0))"
          sz  = 100 :+: 100
          sz2 = 200 :+: 200
          v1  = U.generate (sizeLen sz)
                (\i -> if i<50 then NoData else Value (fromIntegral i))
      ds' <- createMem sz 1 GDT_Int32 []
      setDatasetGeotransform gt ds'
      b <- getBand 1 ds'
      setBandNodataValue (-1) b
      writeBand b (allBand b) sz v1
      flushCache ds'
      ds <- unsafeToReadOnly ds'
      let opts = def
            -- & cutline .~ Just cl
               & transformer .~ SomeTransformer (gipt)

      b2 <- getBand 1 =<< createWarpedVRT ds sz2 gt opts
      v2 <- readBand b2 (allBand b2) sz2
      catValues v2 `shouldSatisfy` U.all (> 0)
      U.sum (catValues v2) `shouldSatisfy` (< U.sum (catValues v1))
-}
    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata (GenImgProjTransformer) " ++ show algo) $ do
        let sz  = 100 :+: 100
            sz2 = 200 :+: 200
            gt  = Geotransform 0 10 0 0 0 (-10)
            v1  = U.generate (sizeLen sz)
                  (\i -> if i<50 then NoData else Value (fromIntegral i))
        ds' <- createMem sz 1 GDT_Int32 []
        setDatasetGeotransform gt ds'
        b <- getBand 1 ds'
        setBandNodataValue (-1) b
        writeBand b (allBand b) sz v1
        ds <- unsafeToReadOnly ds'

        let opts = def
              & resampleAlg .~ algo
              & transformer .~ SomeTransformer gipt
        b2 <- getBand 1 =<< createWarpedVRT ds sz2 gt opts
        v2 <- readBand b2 (allBand b2) sz2
        catValues v2 `shouldSatisfy` U.all (> 0)
        U.sum (catValues v2) `shouldBe` (U.sum (catValues v1))

    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata (GenImgProjTransformer2) " ++ show algo) $ do
        let sz  = 100 :+: 100
            sz2 = 200 :+: 200
            gt  = Geotransform 0 10 0 0 0 (-10)
            v1  = U.generate (sizeLen sz)
                  (\i -> if i<50 then NoData else Value (fromIntegral i))
        ds' <- createMem sz 1 GDT_Int32 []
        setDatasetGeotransform gt ds'
        b <- getBand 1 ds'
        setBandNodataValue (-1) b
        writeBand b (allBand b) sz v1
        ds <- unsafeToReadOnly ds'

        let opts = def
              & resampleAlg .~ algo
              & transformer .~ SomeTransformer gipt2
        b2 <- getBand 1 =<< createWarpedVRT ds sz2 gt opts
        v2 <- readBand b2 (allBand b2) sz2
        catValues v2 `shouldSatisfy` U.all (> 0)
        U.sum (catValues v2) `shouldBe` (U.sum (catValues v1))


    forM_ resampleAlgorithmsWhichHandleNodata $ \algo ->
      it ("handles nodata (GenImgProjTransformer3) " ++ show algo) $ do
        let sz  = 100 :+: 100
            sz2 = 200 :+: 200
            gt  = Geotransform 0 10 0 0 0 (-10)
            v1  = U.generate (sizeLen sz)
                  (\i -> if i<50 then NoData else Value (fromIntegral i))
        ds' <- createMem sz 1 GDT_Int32 []
        setDatasetGeotransform gt ds'
        b <- getBand 1 ds'
        setBandNodataValue (-1) b
        writeBand b (allBand b) sz v1
        ds <- unsafeToReadOnly ds'

        let opts = def
              & resampleAlg .~ algo
              & transformer .~ SomeTransformer gipt3
        b2 <- getBand 1 =<< createWarpedVRT ds sz2 gt opts
        v2 <- readBand b2 (allBand b2) sz2
        catValues v2 `shouldSatisfy` U.all (> 0)
        U.sum (catValues v2) `shouldBe` (U.sum (catValues v1))

resampleAlgorithmsWhichHandleNodata :: [ResampleAlg]
resampleAlgorithmsWhichHandleNodata
  = filter (`notElem` bad) [minBound..maxBound]
  where
    bad
      | GDAL.version >= (1, 11) = [CubicSpline]
      | otherwise               = [CubicSpline, Mode, Average]
