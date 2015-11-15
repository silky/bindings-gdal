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
  [|GDT_Byte|]
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
  [|GDT_UInt16|]
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
  [|GDT_UInt32|]
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
  [|GDT_Byte|]
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
  [|GDT_Int16|]
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
  [|GDT_Int32|]
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
  [|GDT_Float32|]
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
  [|GDT_Float64|]
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
  [|GDT_CInt16|]
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
  [|GDT_CInt32|]
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
  [|GDT_CFloat32|]
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
  [|GDT_CFloat64|]
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
  [|GDT_CInt16|]
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
  [|GDT_CInt32|]
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
  [|GDT_CFloat32|]
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
  [|GDT_CFloat64|]
  [|truncate . realPart|]
  [|\w->fromIntegral w :+ 0|]
  [|realToFrac . realPart|]
  [|\w->realToFrac w :+ 0|]
  [|fmap truncate . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap fromIntegral|]
  [|fmap realToFrac . Pair . (realPart &&& imagPart)|]
  [|uncurry (:+) . unPair . fmap realToFrac|]
{-
deriveGDALType "CDouble"
  [t|CDouble|]
  [|GDT_Float64|]
  [|truncate|]
  [|fromIntegral|]
  [|realToFrac|]
  [|realToFrac|]
  [|(\w->Pair(truncate w,0))|]
  [|(fromIntegral.fst.unPair)|]
  [|\w->Pair(realToFrac w,0)|]
  [|(realToFrac.fst.unPair)|]
-}
