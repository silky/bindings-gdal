{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}

module GDAL.Internal.Vector.Masked (
    Nullable (..)
  , Vector (..)
  , MVector(..)
  , Mask (..)
  , Masked
  , IOVector
  , STVector
  , newWithMask
  , newWithMaskM
  , newWithNoData
  , newWithNoDataM
  , newAllValid
  , newAllValidM
  , newAllValidWithMaskM
  , toBaseVector
  , toBaseVectorWithNoData
  , toBaseVectorWithMask
) where

import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM, liftM2)
import Control.Monad.Primitive

import Data.Maybe -- FIXME
import Text.Read     ( Read(..), readListPrecDefault )
import Data.Typeable (Typeable)
import Data.Word

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable         as St
import qualified Data.Vector.Fusion.Bundle as Bundle

import GHC.Exts (IsList(..), inline)


class Eq (Elem f) => Nullable f where
  type Elem  f      :: *
  nullElem          :: f
  isNull            :: f -> Bool
  toNullable        :: Elem f -> f
  fromNullable      :: Elem f -> f -> Elem f

instance Eq a => Nullable (Maybe a) where
  type Elem (Maybe a)       = a
  nullElem = Nothing
  toNullable = Just
  fromNullable = fromMaybe
  isNull = isNothing


type Masked v a = ( Nullable a
                  , G.Vector v (Elem a)
                  , M.MVector (G.Mutable v) (Elem a)
                  )




maskValid, maskNoData :: Word8
maskValid  = 255
maskNoData = 0

data Mask v a
  = AllValid
  | Mask      (v Word8)
  | UseNoData a

type instance G.Mutable (Vector v) = MVector (G.Mutable v)

newtype Vector v a =
  Vector (Mask St.Vector (Elem a), v (Elem a))
  deriving Typeable

instance (NFData (Elem a), NFData (v (Elem a))) => NFData (Vector v a) where
  rnf (Vector (Mask m,v)) = rnf m `seq` rnf v `seq` ()
  rnf (Vector (UseNoData nd,v)) = rnf nd `seq` rnf v `seq` ()
  rnf (Vector (AllValid,v)) = rnf v `seq` ()

instance (Masked v a, Show a) => Show (Vector v a) where
  showsPrec = G.showsPrec

instance (Masked v a, Read a) => Read (Vector v a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault


instance (Masked v a, Eq a) => Eq (Vector v a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))

instance (Masked v a, Ord a) => Ord (Vector v a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT

instance Masked v a => IsList (Vector v a) where
  type Item (Vector v a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList

newWithMask
  :: Masked v a => St.Vector Word8 -> v (Elem a) -> Vector v a
newWithMask mask values = Vector (Mask mask, values)

newAllValid
  :: Masked v a => v (Elem a) -> Vector v a
newAllValid values = Vector (AllValid, values)

newWithNoData
  :: Masked v a => (Elem a) -> v (Elem a) -> Vector v a
newWithNoData a values = Vector (UseNoData a, values)

newtype MVector v s a =
  MVector (Mask (St.MVector s) (Elem a), v s (Elem a))
  deriving Typeable

type IOVector v = MVector v RealWorld
type STVector v s = MVector v s

instance ( NFData (Elem a)
         , NFData (v s (Elem a))
         ) => NFData (MVector v s a) where
  rnf (MVector (Mask m,v)) = rnf m `seq` rnf v `seq` ()
  rnf (MVector (UseNoData nd,v)) = rnf nd `seq` rnf v `seq` ()
  rnf (MVector (AllValid,v)) = rnf v `seq` ()

newWithMaskM
  :: Nullable a
  => St.MVector s Word8
  -> v s (Elem a)
  -> MVector v s a
newWithMaskM mask values = MVector (Mask mask, values)
{-# INLINE newWithMaskM #-}

newAllValidM
  :: Nullable a => v s (Elem a) -> MVector v s a
newAllValidM values = MVector (AllValid, values)
{-# INLINE newAllValidM #-}

newAllValidWithMaskM
  :: (PrimMonad m, Nullable a, M.MVector v (Elem a))
  => v (PrimState m) (Elem a)
  -> m (MVector v (PrimState m) a)
newAllValidWithMaskM values = do
  mask <- M.replicate (M.length values) maskValid
  return (MVector (Mask mask, values))
{-# INLINE newAllValidWithMaskM #-}

newWithNoDataM
  :: Nullable a => Elem a -> v s (Elem a) -> MVector v s a
newWithNoDataM a values = MVector (UseNoData a, values)

{-# INLINE newWithNoDataM #-}


toBaseVectorWithNoData
  :: Masked v a => Elem a -> Vector v a -> v (Elem a)
toBaseVectorWithNoData nd v =
   (G.generate (G.length v) (fromNullable nd . G.unsafeIndex v))

toBaseVectorWithMask
  :: (Nullable a, G.Vector v (Elem a))
  => Vector v a
  -> (St.Vector Word8, v (Elem a))
toBaseVectorWithMask (Vector (Mask m  , vs)) = (m, vs)
toBaseVectorWithMask (Vector (AllValid, vs)) =
  ((G.replicate (G.length vs) maskValid), vs)
toBaseVectorWithMask (Vector (UseNoData nd, vs)) =
  (G.generate (G.length vs)
   (\i-> if vs `G.unsafeIndex` i==nd then maskNoData else maskValid), vs)

toBaseVector
  :: (Nullable a, G.Vector v (Elem a))
  => Vector v a -> Maybe (v (Elem a))
toBaseVector (Vector (AllValid, v)) = Just v
toBaseVector (Vector (UseNoData nd, v))
  | G.any (==nd) v = Nothing
  | otherwise      = Just v
toBaseVector (Vector (Mask m, v))
  | G.any (==maskNoData) m = Nothing
  | otherwise              = Just ( v)



instance Masked v a => G.Vector (Vector v) a where
  basicUnsafeFreeze (MVector (Mask x,v)) =
    liftM2 (\x' v' -> Vector (Mask x',v'))
           (G.basicUnsafeFreeze x)
           (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MVector (AllValid,v)) =
    liftM (\v' -> Vector (AllValid,v')) (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MVector (UseNoData nd,v)) =
    liftM (\v' -> Vector (UseNoData nd,v')) (G.basicUnsafeFreeze v)
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (Vector (Mask x,v)) =
    liftM2 (\x' v' -> MVector (Mask x',v'))
           (G.basicUnsafeThaw x)
           (G.basicUnsafeThaw v)
  basicUnsafeThaw (Vector (AllValid,v)) =
    liftM (\v' -> MVector (AllValid,v')) (G.basicUnsafeThaw v)
  basicUnsafeThaw (Vector (UseNoData nd,v)) =
    liftM (\v' -> MVector (UseNoData nd,v')) (G.basicUnsafeThaw v)
  {-# INLINE basicUnsafeThaw #-}

  basicLength  (Vector (_,v)) = G.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (Vector (Mask x,v)) =
    Vector (Mask (G.basicUnsafeSlice m n x), G.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (Vector (x,v)) =
    Vector (x, G.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (Vector (Mask x,v)) i = do
    m <- G.basicUnsafeIndexM x i
    if m/=maskNoData
       then liftM toNullable (G.basicUnsafeIndexM v i)
       else return nullElem
  basicUnsafeIndexM (Vector (AllValid,v)) i =
     liftM toNullable (G.basicUnsafeIndexM v i)
  basicUnsafeIndexM (Vector (UseNoData nd,v)) i = do
    val <- G.basicUnsafeIndexM v i
    return (if val==nd then nullElem else toNullable val)
  {-# INLINE basicUnsafeIndexM #-}



instance (Nullable a, M.MVector v (Elem a)) => M.MVector (MVector v) a where
  basicLength (MVector (_,v)) = M.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MVector (Mask x,v)) =
    MVector ( Mask (M.basicUnsafeSlice m n x)
             , M.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (MVector (x,v)) =
    MVector (x, M.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (MVector (_,v)) (MVector (_,v')) = M.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew i =
    liftM2 newWithMaskM (M.basicUnsafeNew i) (M.basicUnsafeNew i)
  {-# INLINE basicUnsafeNew #-}


  basicUnsafeRead (MVector (Mask x,v)) i = do
    m <- M.basicUnsafeRead x i
    if m/=maskNoData
       then liftM toNullable (inline M.basicUnsafeRead v i)
       else return nullElem
  basicUnsafeRead (MVector (AllValid,v)) i =
    liftM toNullable (inline M.basicUnsafeRead v i)
  basicUnsafeRead (MVector (UseNoData nd,v)) i = do
    val <- inline M.basicUnsafeRead v i
    return (if val == nd then nullElem else toNullable val)
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (MVector (Mask x,v)) i a
    | isNull a  = M.basicUnsafeWrite x i maskNoData
    | otherwise = do
        M.basicUnsafeWrite x i maskValid
        M.basicUnsafeWrite v i (fromNullable unexpectedNull a)
  basicUnsafeWrite (MVector (AllValid,v)) i a
    | isNull a  = error ("GDAL.Vector.Masked.basicUnsafeWrite: " ++
                         "tried to write a nullValue in an AllValid vector")
    | otherwise = M.basicUnsafeWrite v i (fromNullable unexpectedNull a)
  basicUnsafeWrite (MVector (UseNoData nd,v)) i a =
    M.basicUnsafeWrite v i (fromNullable nd a)
  {-# INLINE basicUnsafeWrite #-}

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MVector (Mask x,v)) = do
    M.basicInitialize x
    M.basicInitialize v
  basicInitialize (MVector (_,v)) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

unexpectedNull :: t
unexpectedNull =
  error "GDAL.Vector.Masked: Unexpected null element from fromNullable"
