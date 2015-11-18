{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

{#context lib = "gdal" prefix = "GDAL" #}
#include "gdal.h"

module GDAL.Band.Generic (
    Band (..)
  , ROBand
  , RWBand
  , BandVector (..)
  , BandMVector (..)
  , Vector
  , MVector
  , IOVector
  , STVector
  , bandCount
  , bandDataType
  , bandBlockSize
  , bandBlockCount
  , bandBlockLen
  , bandMaskType
  , bandSize
  , bandHasOverviews
  , bandTypedAs
  , allBand
  , bandNodataValue
  , setBandNodataValue
  , getBand
  , addBand
  , fillBand
  , createMaskBand
  , copyBand
  , foldl'
  , foldlM'
  , ifoldl'
  , ifoldlM'
) where

{#import GDAL.Internal.GDAL.Types#}

import GDAL.Internal.DataType (GDALType(toDouble,fromDouble), DataType)
import GDAL.Internal.DataType.Instances()
import GDAL.Band.Generic.Internal (
    setBandNodataValueAsDouble
  , bandNodataValueAsDouble
  , getBandH
  )
import GDAL.Internal.Types
import GDAL.Internal.Util
{#import GDAL.Internal.CPLError#}
{#import GDAL.Internal.CPLString#}
{#import GDAL.Internal.CPLProgress#}


import Control.Applicative (liftA2, pure)
import Control.Monad (liftM, liftM2)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Primitive (RealWorld)

import Data.Bits ((.&.))
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import Foreign.C.Types
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)

import GHC.Types (SPEC(..))
import GHC.Exts (inline)

import System.IO.Unsafe (unsafePerformIO)



type IOVector b = MVector b RealWorld
type STVector b = MVector b

data family Vector  (b :: * -> * -> AccessMode -> *)   a
data family MVector (b :: * -> * -> AccessMode -> *) s a

type instance G.Mutable (Vector b) = (MVector b)

class M.MVector v a => BandMVector v a where
  unsafeWithM :: v RealWorld a -> (Ptr a -> IO c) -> IO c

class ( M.MVector   (G.Mutable v) a
      , BandMVector (G.Mutable v) a
      , G.Vector    v             a
      ) => BandVector v a where
  unsafeWith :: v a -> (Ptr a -> IO c) -> IO c




type ROBand b s a = Band b s a ReadOnly
type RWBand b s a = Band b s a ReadWrite

class ( MajorObject (b s a) t
      , G.Vector (Vector b) a
      , M.MVector (MVector b) a
      ) => Band b s a (t::AccessMode) where

  bandH      :: b s a t -> BandH
  fromBandH  :: BandH -> b s a t

  validate :: b s a t -> GDAL s ()
  validate = const (return ())

  readWindow :: b s a t -> Envelope Int -> Size -> GDAL s (Vector b a)

  writeWindow
    :: t ~ ReadWrite
    => b s a t
    -> Envelope Int
    -> Size
    -> Vector b a
    -> GDAL s ()

  writeBlock :: t ~ ReadWrite => b s a t -> BlockIx  -> Vector b a -> GDAL s ()

  readBlock :: b s a t -> BlockIx -> GDAL s (Vector b a)
  readBlock band blockIx = do
    (vec, load) <- inline blockLoader band
    load blockIx
    liftIO $ G.unsafeFreeze vec
  {-# INLINE readBlock #-}

  blockLoader :: b s a t -> GDAL s (IOVector b a, BlockIx -> GDAL s ())

bandCount :: Dataset s t -> GDAL s Int
bandCount =
  liftM fromIntegral . liftIO . {#call GetRasterCount as ^#} . unDataset

getBand :: Band b s a t => Int -> Dataset s t -> GDAL s (b s a t)
getBand n ds = do
  b <- liftM fromBandH (liftIO (getBandH n (unDataset ds)))
  validate b
  return b

addBand
  :: RWBand b s a
  => RWDataset s -> DataType -> OptionList -> GDAL s (b s a ReadWrite)
addBand ds dt options = do
  liftIO $
    checkCPLError "addBand" $
    withOptionList options $
    {#call GDALAddBand as ^#} (unDataset ds) (fromEnumC dt)
  ix <- bandCount ds
  getBand ix ds

fillBand :: RWBand b s a => a -> b s a ReadWrite -> GDAL s ()
fillBand v band =
  mapM_ (\i -> writeBlock band i vec) [XY i j | j<-[0..ny-1], i<-[0..nx-1]]
  where
    XY nx ny = bandBlockCount band
    vec      = G.replicate (bandBlockLen band) v

bandDataType :: Band b s a t => b s a t -> DataType
bandDataType = toEnumC . {#call pure unsafe GetRasterDataType as ^#} . bandH

bandBlockSize :: Band b s a t => b s a t -> Size
bandBlockSize band = unsafePerformIO $ alloca $ \xPtr -> alloca $ \yPtr -> do
   {#call unsafe GetBlockSize as ^#} (bandH band) xPtr yPtr
   liftM (fmap fromIntegral) (liftM2 XY (peek xPtr) (peek yPtr))

bandBlockLen :: Band b s a t => b s a t -> Int
bandBlockLen = (\(XY x y) -> x*y) . bandBlockSize


bandSize :: Band b s a t => b s a t -> Size
bandSize band =
  fmap fromIntegral $
    XY ({#call pure unsafe GetRasterBandXSize as ^#} (bandH band))
       ({#call pure unsafe GetRasterBandYSize as ^#} (bandH band))

allBand :: Band b s a t => b s a t -> Envelope Int
allBand = Envelope (pure 0) . bandSize

bandBlockCount :: Band b s a t => b s a t -> XY Int
bandBlockCount b = fmap ceiling $ liftA2 ((/) :: Double -> Double -> Double)
                     (fmap fromIntegral (bandSize b))
                     (fmap fromIntegral (bandBlockSize b))

bandHasOverviews :: Band b s a t => b s a t -> GDAL s Bool
bandHasOverviews =
  liftIO . liftM toBool . {#call unsafe HasArbitraryOverviews as ^#} . bandH


bandNodataValue
  :: (GDALType a , Band b s a t) => b s a t -> GDAL s (Maybe a)
bandNodataValue =
  liftM (fmap fromDouble) . liftIO . bandNodataValueAsDouble . bandH


setBandNodataValue
  :: (GDALType a, RWBand b s a) => b s a ReadWrite -> a -> GDAL s ()
setBandNodataValue b = liftIO . setBandNodataValueAsDouble (bandH b) . toDouble

createMaskBand :: RWBand b s a => b s a ReadWrite -> MaskType -> GDAL s ()
createMaskBand band maskType = liftIO $
  checkCPLError "createBandMask" $
  {#call GDALCreateMaskBand as ^#} (bandH band) cflags
  where cflags = maskFlagsForType maskType



copyBand
  :: (Band b s a t, RWBand b2 s a)
  => b  s a t
  -> b2 s a ReadWrite
  -> OptionList
  -> Maybe ProgressFun -> GDAL s ()
copyBand src dst options progressFun =
  liftIO $
  withProgressFun "copyBand" progressFun $ \pFunc ->
  withOptionList options $ \o ->
  checkCPLError "copyBand" $
  {#call RasterBandCopyWholeRaster as ^#}
    (bandH src) (bandH dst) o pFunc nullPtr



bandMaskType :: Band b s a t => b s a t -> MaskType
bandMaskType band = unsafePerformIO $ do
  flags <- {#call unsafe GetMaskFlags as ^#} (bandH band)
  let testFlag f = (f .&. flags) /= 0
  return $ case () of
    () | testFlag {#const GMF_NODATA      #} -> MaskNoData
    () | testFlag {#const GMF_ALL_VALID   #} -> MaskAllValid
    () | testFlag {#const GMF_PER_DATASET #} -> MaskPerDataset
    _                                        -> MaskPerBand
{-# INLINE bandMaskType #-}

bandTypedAs :: Band b s a t => b s a t -> a -> b s a t
bandTypedAs b _ = b
{-# INLINE bandTypedAs #-}


foldl'
  :: Band b s a t
  => (z -> a -> z) -> z -> b s a t -> GDAL s z
foldl' f = foldlM' (\acc -> return . f acc)
{-# INLINE foldl' #-}

ifoldl'
  :: Band b s a t
  => (z -> XY Int -> a -> z) -> z -> b s a t -> GDAL s z
ifoldl' f = ifoldlM' (\acc ix -> return . f acc ix)
{-# INLINE ifoldl' #-}

foldlM'
  :: Band b s a t
  => (z -> a -> GDAL s z) -> z -> b s a t -> GDAL s z
foldlM' f = ifoldlM' (\acc _ -> f acc)
{-# INLINE foldlM' #-}

ifoldlM'
  :: Band b s a t
  => (z -> XY Int -> a -> GDAL s z) -> z -> b s a t -> GDAL s z
ifoldlM' f initialAcc band = do
  (vec,load) <- inline blockLoader band
  ifoldlM_loop SPEC vec load
  where
    !(XY mx my) = liftA2 mod (bandSize band) (bandBlockSize band)
    !(XY nx ny) = bandBlockCount band
    !(XY sx sy) = bandBlockSize band
    {-# INLINE ifoldlM_loop #-}
    ifoldlM_loop !_ vec load = loop1 SPEC initialAcc 0 0
      where
        {-# INLINE loop1 #-}
        loop1 !_ !acc !iB !jB
          | iB < nx  = do load (XY iB jB)
                          acc' <- loop2 SPEC acc 0 0
                          loop1 SPEC acc' (iB+1) jB
          | jB+1 < ny = loop1 SPEC acc 0 (jB+1)
          | otherwise = return acc
          where
            {-# INLINE loop2 #-}
            loop2 !_ !acc' !i !j
              | i   < stopx = do
                  v <- liftIO (M.unsafeRead vec (j*sx+i))
                  acc'' <- f acc' ix v
                  loop2 SPEC acc'' (i+1) j
              | j+1 < stopy = loop2 SPEC acc' 0 (j+1)
              | otherwise   = return acc'
              where ix = XY (iB*sx+i) (jB*sy+j)
            !stopx
              | mx /= 0 && iB==nx-1 = mx
              | otherwise           = sx
            !stopy
              | my /= 0 && jB==ny-1 = my
              | otherwise           = sy
{-# INLINE ifoldlM' #-}
