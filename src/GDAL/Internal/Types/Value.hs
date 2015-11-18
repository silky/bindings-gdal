{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GDAL.Internal.Types.Value (
    Value(..)
  , isNoData
  , fromValue
) where

import GDAL.Internal.Vector.Masked (Nullable (..), Masked)

import Control.Applicative (Applicative(..), (<$>))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)

import Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M



data Value a
  = Value {unValue :: {-# UNPACK #-} !a}
  | NoData
  deriving (Eq, Ord, Show, Read, Typeable)

instance NFData a => NFData (Value a) where
  rnf (Value a) = rnf a `seq` ()
  rnf NoData    = ()
  {-# INLINE rnf #-}

instance Functor Value where
  fmap _ NoData       = NoData
  fmap f (Value a)    = Value (f a)
  {-# INLINE fmap #-}

instance Applicative Value where
  pure = Value
  {-# INLINE pure #-}

  Value f <*> m       = fmap f m
  NoData  <*> _m      = NoData
  {-# INLINE (<*>) #-}

  Value _m1 *> m2     = m2
  NoData    *> _m2    = NoData
  {-# INLINE (*>) #-}

instance Monad Value where
  (Value x) >>= k     = k x
  NoData    >>= _     = NoData
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

  return              = Value
  {-# INLINE return #-}
  fail _              = NoData
  {-# INLINE fail #-}

instance Num a => Num (Value a) where
  Value a + Value b = Value (a+b)
  Value a + NoData  = Value a
  NoData  + Value a = Value a
  NoData  + NoData  = NoData
  {-# INLINE (+) #-}

  Value a - Value b = Value (a-b)
  Value a - NoData  = Value a
  NoData  - Value a = Value a
  NoData  - NoData  = NoData
  {-# INLINE (-) #-}

  Value a * Value b = Value (a*b)
  Value a * NoData  = Value a
  NoData  * Value a = Value a
  NoData  * NoData  = NoData
  {-# INLINE (*) #-}

  negate = fmap negate
  {-# INLINE negate #-}

  abs = fmap abs
  {-# INLINE abs #-}

  signum = fmap signum
  {-# INLINE signum #-}

  fromInteger = Value . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Value a) where
  Value a / Value b = Value (a/b)
  Value a / NoData  = Value a
  NoData  / Value a = Value a
  NoData  / NoData  = NoData
  {-# INLINE (/) #-}

  recip = fmap recip
  {-# INLINE recip #-}

  fromRational = Value . fromRational
  {-# INLINE fromRational #-}

isNoData :: Value a -> Bool
isNoData NoData = True
isNoData _      = False
{-# INLINE isNoData #-}

fromValue :: a -> Value a -> a
fromValue v NoData    = v
fromValue _ (Value v) = v
{-# INLINE fromValue #-}

instance Eq a => Nullable (Value a) where
  type Elem (Value a)       = a
  nullElem = NoData
  {-# INLINE nullElem #-}
  toNullable = Value
  {-# INLINE toNullable #-}
  fromNullable = fromValue
  {-# INLINE fromNullable #-}
  isNull = isNoData
  {-# INLINE isNull #-}

{-

newtype instance U.MVector s (Value a) =
  UMV_Masked (MV.MVector U.MVector s (Value a))
newtype instance U.Vector    (Value a) =
  UV_Masked  (MV.Vector  U.Vector    (Value a))
instance (Eq a, U.Unbox a) => U.Unbox (Value a)

instance (U.Unbox a, Eq a) => M.MVector U.MVector (Value a) where
  {-# INLINE basicLength #-}
  basicLength (UMV_Masked v) = M.basicLength v

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (UMV_Masked v) = UMV_Masked (M.basicUnsafeSlice j m v)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (UMV_Masked a) (UMV_Masked b) = M.basicOverlaps a b

  basicUnsafeNew i = liftM UMV_Masked (M.basicUnsafeNew i)
  {-# INLINE basicUnsafeNew #-}

#if MIN_VERSION_vector(0,11,0)
  {-# INLINE basicInitialize #-}
  basicInitialize (UMV_Masked v) = M.basicInitialize v
#endif

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (UMV_Masked v) i = M.basicUnsafeRead v i

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (UMV_Masked v) i a = M.basicUnsafeWrite v i a

  {-# INLINE basicSet #-}
  basicSet (UMV_Masked v) x = M.basicSet v x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (UMV_Masked a) (UMV_Masked b) = M.basicUnsafeCopy a b

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (UMV_Masked a) (UMV_Masked b) = M.basicUnsafeMove a b



instance (U.Unbox a, Eq a) => G.Vector (U.Vector) (Value a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (UMV_Masked mv) = liftM UV_Masked (G.basicUnsafeFreeze mv)

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (UV_Masked v) = liftM UMV_Masked (G.basicUnsafeThaw v)

  {-# INLINE basicLength #-}
  basicLength (UV_Masked v) = G.basicLength v

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice n m (UV_Masked v) = UV_Masked (G.basicUnsafeSlice n m v)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (UV_Masked v) i = G.basicUnsafeIndexM v i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (UMV_Masked mv) (UV_Masked v) = G.basicUnsafeCopy mv v

  {-# INLINE elemseq #-}
  elemseq _ = seq
-}
