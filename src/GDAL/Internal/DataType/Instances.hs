{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module GDAL.Internal.DataType.Instances () where

import GDAL.Internal.DataType
import GDAL.Internal.DataType.TH
import GDAL.Internal.Types.Pair

import Control.Arrow

import Data.Int
import Data.Word
import GHC.Prim

#if MIN_VERSION_base(4,8,0)
import Data.Complex (Complex((:+)), realPart, imagPart)
#else
import Data.Complex (Complex((:+)))
realPart, imagPart :: Complex t -> t
realPart (a :+ _) = a
imagPart (_ :+ a) = a
#endif

deriveGDALType "Word8"
  [t|Word8|]
  [|gdtByte|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|truncate|]
  [|(\w->Pair(fromIntegral w,0))|]
  [|(fromIntegral.fst.unPair)|]
  [|\w->Pair(fromIntegral w,0)|]
  [|(truncate.fst.unPair)|]
deriveGDALType "Word16"
  [t|Word16|]
  [|gdtUInt16|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|truncate|]
  [|(\w->Pair(fromIntegral w,0))|]
  [|(fromIntegral.fst.unPair)|]
  [|\w->Pair(fromIntegral w,0)|]
  [|(truncate.fst.unPair)|]
deriveGDALType "Word32"
  [t|Word32|]
  [|gdtUInt32|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|truncate|]
  [|(\w->Pair(fromIntegral w,0))|]
  [|(fromIntegral.fst.unPair)|]
  [|\w->Pair(fromIntegral w,0)|]
  [|(truncate.fst.unPair)|]
deriveGDALType "Int8"
  [t|Int8|]
  [|gdtByte|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|truncate|]
  [|(\w->Pair(fromIntegral w,0))|]
  [|(fromIntegral.fst.unPair)|]
  [|\w->Pair(fromIntegral w,0)|]
  [|(truncate.fst.unPair)|]
deriveGDALType "Int16"
  [t|Int16|]
  [|gdtInt16|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|truncate|]
  [|(\w->Pair(fromIntegral w,0))|]
  [|(fromIntegral.fst.unPair)|]
  [|\w->Pair(fromIntegral w,0)|]
  [|(truncate.fst.unPair)|]
deriveGDALType "Int32"
  [t|Int32|]
  [|gdtInt32|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|fromIntegral|]
  [|truncate|]
  [|(\w->Pair(fromIntegral w,0))|]
  [|(fromIntegral.fst.unPair)|]
  [|\w->Pair(fromIntegral w,0)|]
  [|(truncate.fst.unPair)|]
deriveGDALType "Float"
  [t|Float|]
  [|gdtFloat32|]
  [|truncate|]
  [|fromIntegral|]
  [|realToFrac|]
  [|realToFrac|]
  [|(\w->Pair(truncate w,0))|]
  [|(fromIntegral.fst.unPair)|]
  [|\w->Pair(realToFrac w,0)|]
  [|(realToFrac.fst.unPair)|]
deriveGDALType "Double"
  [t|Double|]
  [|gdtFloat64|]
  [|truncate|]
  [|fromIntegral|]
  [|realToFrac|]
  [|realToFrac|]
  [|(\w->Pair(truncate w,0))|]
  [|(fromIntegral.fst.unPair)|]
  [|\w->Pair(realToFrac w,0)|]
  [|(realToFrac.fst.unPair)|]
deriveGDALType "PairInt16"
  [t|Pair Int16|]
  [|gdtCInt16|]
  [|fromIntegral . fst .unPair|]
  [|\w->Pair (fromIntegral w, 0)|]
  [|fromIntegral . fst .unPair|]
  [|\w->Pair (truncate w, 0)|]
  [|fmap fromIntegral|]
  [|fmap fromIntegral|]
  [|fmap fromIntegral|]
  [|fmap truncate|]
deriveGDALType "PairInt32"
  [t|Pair Int32|]
  [|gdtCInt32|]
  [|fromIntegral . fst .unPair|]
  [|\w->Pair (fromIntegral w, 0)|]
  [|fromIntegral . fst .unPair|]
  [|\w->Pair (truncate w, 0)|]
  [|fmap fromIntegral|]
  [|fmap fromIntegral|]
  [|fmap fromIntegral|]
  [|fmap truncate|]
deriveGDALType "PairFloat"
  [t|Pair Float|]
  [|gdtCFloat32|]
  [|truncate . fst .unPair|]
  [|\w->Pair (fromIntegral w, 0)|]
  [|realToFrac . fst .unPair|]
  [|\w->Pair (realToFrac w, 0)|]
  [|fmap truncate|]
  [|fmap fromIntegral|]
  [|fmap realToFrac|]
  [|fmap realToFrac|]
deriveGDALType "PairDouble"
  [t|Pair Double|]
  [|gdtCFloat64|]
  [|truncate . fst .unPair|]
  [|\w->Pair (fromIntegral w, 0)|]
  [|realToFrac . fst .unPair|]
  [|\w->Pair (realToFrac w, 0)|]
  [|fmap truncate|]
  [|fmap fromIntegral|]
  [|fmap realToFrac|]
  [|fmap realToFrac|]

deriveGDALType "ComplexInt16"
  [t|Complex Int16|]
  [|gdtCInt16|]
  [|fromIntegral . realPart|]
  [|\w->fromIntegral w :+ 0|]
  [|fromIntegral . realPart|]
  [|\w->truncate w :+ 0|]
  [|fmap fromIntegral . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap fromIntegral|]
  [|fmap fromIntegral . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap truncate|]
deriveGDALType "ComplexInt32"
  [t|Complex Int32|]
  [|gdtCInt32|]
  [|fromIntegral . realPart|]
  [|\w->fromIntegral w :+ 0|]
  [|fromIntegral . realPart|]
  [|\w->truncate w :+ 0|]
  [|fmap fromIntegral . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap fromIntegral|]
  [|fmap fromIntegral . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap truncate|]
deriveGDALType "ComplexFloat"
  [t|Complex Float|]
  [|gdtCFloat32|]
  [|truncate . realPart|]
  [|\w->fromIntegral w :+ 0|]
  [|realToFrac . realPart|]
  [|\w->realToFrac w :+ 0|]
  [|fmap truncate . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap fromIntegral|]
  [|fmap realToFrac . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap realToFrac|]
deriveGDALType "ComplexDouble"
  [t|Complex Double|]
  [|gdtCFloat64|]
  [|truncate . realPart|]
  [|\w->fromIntegral w :+ 0|]
  [|realToFrac . realPart|]
  [|\w->realToFrac w :+ 0|]
  [|fmap truncate . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap fromIntegral|]
  [|fmap realToFrac . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap realToFrac|]
