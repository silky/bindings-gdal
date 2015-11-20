{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module GDAL.Internal.Types.Vector.Masked (
    Nullable (..)
  , Masked (..)
  , NullableVector (..)
  , BaseMVector
  , Vector
  , MVector
  , IOVector
  , ElemVector
  , ElemMVector
  , deriveNullableVector
) where

import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM, liftM2)
import Control.Monad.Primitive

import Text.Read     ( Read(..), readListPrecDefault )
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Data.Word

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable         as St
#if MIN_VERSION_vector(0,11,0)
import qualified Data.Vector.Fusion.Bundle as Bundle
#else
import qualified Data.Vector.Fusion.Stream as Stream
#endif

import GHC.Exts (IsList(..), inline)
import GHC.Exts (inline)

import Language.Haskell.TH

type BaseMVector a = G.Mutable (BaseVector a)

class ( G.Vector  (BaseVector  a) a
      , M.MVector (BaseMVector a) a
      , Eq a
      ) => Masked a where
  type BaseVector  a :: * -> *

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
  {-# INLINE nullElem #-}
  {-# INLINE isNull #-}
  {-# INLINE toNullable #-}
  {-# INLINE fromNullable #-}


type ElemVector    f  = BaseVector  (Elem f)   (Elem f)
type ElemMVector s f  = BaseMVector (Elem f) s (Elem f)

data family Vector  a
data family MVector s a

type IOVector = MVector RealWorld

type instance G.Mutable Vector = MVector

class ( Nullable f
      , G.Vector Vector f
      , M.MVector MVector f
      , Masked (Elem f)
      ) => NullableVector f where

  newWithMask
    :: St.Vector Word8 -> ElemVector f -> Vector f

  newAllValid
    :: ElemVector f -> Vector f

  newWithNoData
    :: Elem f -> ElemVector f -> Vector f

  newWithMaskM
    :: NullableVector f
    => St.MVector s Word8
    -> ElemMVector s f
    -> MVector s f

  newAllValidM
    :: ElemMVector s f -> MVector s f

  newWithNoDataM
    :: Elem f -> ElemMVector s f -> MVector s f


  toBaseVectorWithNoData
    :: Elem f -> Vector f -> ElemVector f

  toBaseVectorWithMask
    :: Vector f -> (St.Vector Word8, ElemVector f)

  toBaseVector
    :: Vector f -> Maybe (ElemVector f)


-----------------------------

maskValid, maskNoData :: Word8
maskValid  = 255
maskNoData = 0

data Mask v a
  = AllValid
  | Mask      (v Word8)
  | UseNoData a
  deriving Typeable

deriving instance (Eq a, Eq (v Word8)) => Eq (Mask v a)

newtype MaskedVector f =
  MaskedVector (Mask St.Vector (Elem f) , ElemVector f)
  deriving Typeable

newtype MaskedMVector s f =
  MaskedMVector ( Mask (St.MVector s) (Elem f) , ElemMVector s f)
  deriving Typeable

type instance G.Mutable MaskedVector = MaskedMVector

newWithMask_
  :: (Nullable f, Masked (Elem f)) => St.Vector Word8 -> ElemVector f -> MaskedVector f
newWithMask_ mask values = MaskedVector (Mask mask, values)
{-# INLINE newWithMask_ #-}

newAllValid_
  :: (Nullable f, Masked (Elem f)) => ElemVector f -> MaskedVector f
newAllValid_ values = MaskedVector (AllValid, values)
{-# INLINE newAllValid_ #-}

newWithNoData_
  :: (Nullable f, Masked (Elem f)) => Elem f -> ElemVector f -> MaskedVector f
newWithNoData_ a values = MaskedVector (UseNoData a, values)
{-# INLINE newWithNoData_ #-}



newWithMaskM_
  :: (Nullable f, Masked (Elem f))
  => St.MVector s Word8
  -> ElemMVector s f
  -> MaskedMVector s f
newWithMaskM_ mask values = MaskedMVector (Mask mask, values)
{-# INLINE newWithMaskM_ #-}

newAllValidM_
  :: (Nullable f, Masked (Elem f)) => ElemMVector s f -> MaskedMVector s f
newAllValidM_ values = MaskedMVector (AllValid, values)
{-# INLINE newAllValidM_ #-}

newWithNoDataM_
  :: (Nullable f, Masked (Elem f)) => Elem f -> ElemMVector s f -> MaskedMVector s f
newWithNoDataM_ a values = MaskedMVector (UseNoData a, values)
{-# INLINE newWithNoDataM_ #-}


toBaseVectorWithNoData_
  :: (Nullable f, Masked (Elem f)) => Elem f -> MaskedVector f -> ElemVector f
toBaseVectorWithNoData_ nd v =
   (G.generate (G.length v) (fromNullable nd . G.unsafeIndex v))
{-# INLINE toBaseVectorWithNoData_ #-}

toBaseVectorWithMask_
  :: (Nullable f, Masked (Elem f)) => MaskedVector f -> (St.Vector Word8, ElemVector f)
toBaseVectorWithMask_ (MaskedVector (Mask m  , vs)) = (m, vs)
toBaseVectorWithMask_ (MaskedVector (AllValid, vs)) =
  ((G.replicate (G.length vs) maskValid), vs)
toBaseVectorWithMask_ (MaskedVector (UseNoData nd, vs)) =
  (G.generate (G.length vs)
   (\i-> if vs `G.unsafeIndex` i==nd then maskNoData else maskValid), vs)
{-# INLINE toBaseVectorWithMask_ #-}

toBaseVector_
  :: (Nullable f, Masked (Elem f)) => MaskedVector f -> Maybe (ElemVector f)
toBaseVector_ (MaskedVector (AllValid, v)) = Just v
toBaseVector_ (MaskedVector (UseNoData nd, v))
  | G.any (==nd) v = Nothing
  | otherwise      = Just v
toBaseVector_ (MaskedVector (Mask m, v))
  | G.any (==maskNoData) m = Nothing
  | otherwise              = Just ( v)
{-# INLINE toBaseVector_ #-}



instance (Nullable f, Masked (Elem f)) => G.Vector MaskedVector f where
  basicUnsafeFreeze (MaskedMVector (Mask x,v)) =
    liftM2 (\x' v' -> MaskedVector (Mask x',v'))
           (G.basicUnsafeFreeze x)
           (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MaskedMVector (AllValid,v)) =
    liftM (\v' -> MaskedVector (AllValid,v')) (G.basicUnsafeFreeze v)
  basicUnsafeFreeze (MaskedMVector (UseNoData nd,v)) =
    liftM (\v' -> MaskedVector (UseNoData nd,v')) (G.basicUnsafeFreeze v)
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (MaskedVector (Mask x,v)) =
    liftM2 (\x' v' -> MaskedMVector (Mask x',v'))
           (G.basicUnsafeThaw x)
           (G.basicUnsafeThaw v)
  basicUnsafeThaw (MaskedVector (AllValid,v)) =
    liftM (\v' -> MaskedMVector (AllValid,v')) (G.basicUnsafeThaw v)
  basicUnsafeThaw (MaskedVector (UseNoData nd,v)) =
    liftM (\v' -> MaskedMVector (UseNoData nd,v')) (G.basicUnsafeThaw v)
  {-# INLINE basicUnsafeThaw #-}

  basicLength  (MaskedVector (_,v)) = G.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MaskedVector (Mask x,v)) =
    MaskedVector (Mask (G.basicUnsafeSlice m n x), G.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (MaskedVector (x,v)) =
    MaskedVector (x, G.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (MaskedVector (Mask x,v)) i = do
    m <- G.basicUnsafeIndexM x i
    if m/=maskNoData
       then liftM toNullable (G.basicUnsafeIndexM v i)
       else return nullElem
  basicUnsafeIndexM (MaskedVector (AllValid,v)) i =
     liftM toNullable (G.basicUnsafeIndexM v i)
  basicUnsafeIndexM (MaskedVector (UseNoData nd,v)) i = do
    val <- G.basicUnsafeIndexM v i
    return (if val==nd then nullElem else toNullable val)
  {-# INLINE basicUnsafeIndexM #-}



instance (Nullable f, Masked (Elem f)) => M.MVector MaskedMVector f where
  basicLength (MaskedMVector (_,v)) = M.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MaskedMVector (Mask x,v)) =
    MaskedMVector ( Mask (M.basicUnsafeSlice m n x)
             , M.basicUnsafeSlice m n v)
  basicUnsafeSlice m n (MaskedMVector (x,v)) =
    MaskedMVector (x, M.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (MaskedMVector (_,v)) (MaskedMVector (_,v')) = M.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew i =
    liftM2 newWithMaskM_ (M.basicUnsafeNew i) (M.basicUnsafeNew i)
  {-# INLINE basicUnsafeNew #-}


  basicUnsafeRead (MaskedMVector (Mask x,v)) i = do
    m <- M.basicUnsafeRead x i
    if m/=maskNoData
       then liftM toNullable (inline M.basicUnsafeRead v i)
       else return nullElem
  basicUnsafeRead (MaskedMVector (AllValid,v)) i =
    liftM toNullable (inline M.basicUnsafeRead v i)
  basicUnsafeRead (MaskedMVector (UseNoData nd,v)) i = do
    val <- inline M.basicUnsafeRead v i
    return (if val == nd then nullElem else toNullable val)
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (MaskedMVector (Mask x,v)) i a
    | isNull a  = M.basicUnsafeWrite x i maskNoData
    | otherwise = do
        M.basicUnsafeWrite x i maskValid
        M.basicUnsafeWrite v i (fromNullable unexpectedNull a)
  basicUnsafeWrite (MaskedMVector (AllValid,v)) i a
    | isNull a  = error ("GDAL.MaskedVector.Masked.basicUnsafeWrite: " ++
                         "tried to write a nullValue in an AllValid vector")
    | otherwise = M.basicUnsafeWrite v i (fromNullable unexpectedNull a)
  basicUnsafeWrite (MaskedMVector (UseNoData nd,v)) i a =
    M.basicUnsafeWrite v i (fromNullable nd a)
  {-# INLINE basicUnsafeWrite #-}

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MaskedMVector (Mask x,_))        = M.basicInitialize x
  basicInitialize (MaskedMVector (AllValid,v))      = M.basicInitialize v
  basicInitialize (MaskedMVector (UseNoData nd ,v)) = M.basicSet v nd
  {-# INLINE basicInitialize #-}
#endif

unexpectedNull :: t
unexpectedNull =
  error "GDAL.MaskedVector.Masked: Unexpected null element from fromNullable"


------------------------------------------------------------------------------


instance (NFData (Elem a), NFData (ElemVector a))
  => NFData (MaskedVector a) where
  rnf (MaskedVector (Mask m,v)) = rnf m `seq` rnf v `seq` ()
  rnf (MaskedVector (UseNoData nd,v)) = rnf nd `seq` rnf v `seq` ()
  rnf (MaskedVector (AllValid,v)) = rnf v `seq` ()

instance (G.Vector Vector a, Show a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (G.Vector Vector a, Read a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault


#if MIN_VERSION_vector(0,11,0)
instance (G.Vector Vector a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))

instance (G.Vector Vector a, Ord a) => Ord (Vector a) where
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
#else
instance (G.Vector Vector a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Stream.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Stream.eq (G.stream xs) (G.stream ys))

instance (G.Vector Vector a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Stream.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Stream.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Stream.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Stream.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Stream.cmp (G.stream xs) (G.stream ys) /= LT
#endif

instance G.Vector Vector a => IsList (Vector a) where
  type Item (Vector a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList

instance G.Vector Vector a => Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = G.empty

  {-# INLINE mappend #-}
  mappend = (G.++)

  {-# INLINE mconcat #-}
  mconcat = G.concat

------------------------------------------------------------------------------




deriveNullableVector :: String -> TypeQ -> TypeQ -> DecsQ
deriveNullableVector name typeQ baseVectorQ = do
  type_ <- typeQ
  s <- liftM VarT (newName "s")
  let newtypes = [
          NewtypeInstD [] ''Vector [type_]
            (NormalC vName [(NotStrict, ConT ''MaskedVector `AppT` type_)])
            [''NFData, ''Typeable]
        , NewtypeInstD [] ''MVector [s, type_]
          (NormalC mvName [ (NotStrict,
              ConT ''MaskedMVector `AppT` s `AppT` type_)]) [''Typeable]
        ]
      (ConT _ `AppT` elemT) = type_
      elemQ     = return elemT
      vName     = mkName ("V_"++name)
      mvName    = mkName ("MV_"++name)
      vCon      = return (ConE vName)
      mvCon     = return (ConE mvName)
      vConP  v' = return (ConP vName [VarP v'])
      mvConP v' = return (ConP mvName [VarP v'])
      var    v' = return (VarE v')
  v <- newName "vec"
  u <- newName "other_vec"
  instanceDecs <- [d|
    instance Masked $elemQ where {
    ; type BaseVector $elemQ = $baseVectorQ
    };
    instance NullableVector $typeQ => G.Vector Vector $typeQ where {
      {-# INLINE basicUnsafeFreeze #-}
    ; {-# INLINE basicUnsafeThaw #-}
    ; {-# INLINE basicLength #-}
    ; {-# INLINE basicUnsafeSlice #-}
    ; {-# INLINE basicUnsafeIndexM #-}
    ; basicUnsafeFreeze $(mvConP v) =
        liftM $vCon (G.basicUnsafeFreeze $(var v))
    ; basicUnsafeThaw $(vConP v) = liftM $mvCon (G.basicUnsafeThaw $(var v))
    ; basicLength  $(vConP v) = G.basicLength $(var v)
    ; basicUnsafeSlice m n $(vConP v) = $vCon (G.basicUnsafeSlice m n $(var v))
    ; basicUnsafeIndexM $(vConP v) = G.basicUnsafeIndexM $(var v)
    }
    instance NullableVector $typeQ => M.MVector MVector $typeQ where {
      {-# INLINE basicLength #-}
    ; {-# INLINE basicUnsafeSlice #-}
    ; {-# INLINE basicOverlaps #-}
    ; {-# INLINE basicUnsafeNew #-}
    ; {-# INLINE basicUnsafeRead #-}
    ; {-# INLINE basicUnsafeWrite #-}
    ; basicLength $(mvConP v) = M.basicLength $(var v)
    ; basicUnsafeSlice m n $(mvConP v) =
        $mvCon (M.basicUnsafeSlice m n $(var v))
    ; basicOverlaps $(mvConP v) $(mvConP u) = M.basicOverlaps $(var v) $(var u)
    ; basicUnsafeNew = liftM $mvCon . M.basicUnsafeNew
    ; basicUnsafeRead $(mvConP v) = M.basicUnsafeRead $(var v)
    ; basicUnsafeWrite $(mvConP v) i = M.basicUnsafeWrite $(var v) i
#if MIN_VERSION_vector(0,11,0)
    ; basicInitialize $(mvConP v) = M.basicInitialize $(var v)
    ; {-# INLINE basicInitialize #-}
#endif
    }
    instance Nullable $typeQ => NullableVector $typeQ where {
      {-# INLINE newWithMask #-}
    ; {-# INLINE newAllValid #-}
    ; {-# INLINE newWithNoData #-}
    ; {-# INLINE newWithMaskM #-}
    ; {-# INLINE newAllValidM #-}
    ; {-# INLINE newWithNoDataM #-}
    ; {-# INLINE toBaseVectorWithNoData #-}
    ; {-# INLINE toBaseVectorWithMask #-}
    ; {-# INLINE toBaseVector #-}
    ; newWithMask m v'    = $vCon (newWithMask_ m v')
    ; newAllValid v'      = $vCon (newAllValid_ v')
    ; newWithNoData nd v' = $vCon (newWithNoData_ nd v')
    ; newWithMaskM m v'   = $mvCon (newWithMaskM_ m v')
    ; newAllValidM v' = $mvCon (newAllValidM_ v')
    ; newWithNoDataM nd v' = $mvCon (newWithNoDataM_ nd v')
    ; toBaseVectorWithNoData e $(vConP v) = toBaseVectorWithNoData_ e $(var v)
    ; toBaseVectorWithMask $(vConP v) = toBaseVectorWithMask_ $(var v)
    ; toBaseVector $(vConP v) = toBaseVector_ $(var v)
    }
    |]
  return $ concat [instanceDecs, newtypes]
