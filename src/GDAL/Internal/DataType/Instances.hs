{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GDAL.Internal.DataType.Instances () where

import GDAL.Internal.Types.Value (Value)
import GDAL.Internal.Types.Vector.Masked (deriveNullableVector)
import qualified GDAL.Internal.Types.Vector as GV
import GDAL.Internal.Types.Pair (Pair(..))
import GDAL.Internal.DataType

import Control.Arrow ((&&&))

import Data.Int (Int8, Int16, Int32)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Vector.Storable         as St
import Data.Word (Word8, Word16, Word32)


#if MIN_VERSION_base(4,8,0)
import Data.Complex (Complex((:+)), realPart, imagPart)
#else
import Data.Complex (Complex((:+)))
realPart, imagPart :: Complex t -> t
realPart (a :+ _) = a
imagPart (_ :+ a) = a
#endif





#define dynGType(ty)\
instance GDALType (DynType (ty)) where {\
  dataType _        = dataType (Proxy :: Proxy (ty))\
; gToIntegral       = gToIntegral . unDynType\
; gToIntegralPair   = gToIntegralPair . unDynType\
; gToReal           = gToReal . unDynType\
; gToRealPair       = gToRealPair . unDynType\
; gFromIntegral     = DynType . gFromIntegral\
; gFromIntegralPair = DynType . gFromIntegralPair\
; gFromReal         = DynType . gFromReal\
; gFromRealPair     = DynType . gFromRealPair\
; {-# INLINE dataType          #-}\
; {-# INLINE gToIntegral       #-}\
; {-# INLINE gFromIntegral     #-}\
; {-# INLINE gToReal           #-}\
; {-# INLINE gFromReal         #-}\
; {-# INLINE gToIntegralPair   #-}\
; {-# INLINE gFromIntegralPair #-}\
; {-# INLINE gToRealPair       #-}\
; {-# INLINE gFromRealPair     #-}\
};

instance GDALType Word8 where
  dataType _          = gdtByte
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType          #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
deriveNullableVector "Word8" [t|Value Word8|] [t|St.Vector|]
deriveNullableVector "DynWord8" [t|Value (DynType Word8)|] [t|GV.Vector|]
dynGType(Word8)


instance GDALType Word16 where
  dataType          _ = gdtUInt16
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
deriveNullableVector "Word16" [t|Value Word16|] [t|St.Vector|]
deriveNullableVector "DynWord16" [t|Value (DynType Word16)|] [t|GV.Vector|]
dynGType(Word16)

instance GDALType Word32 where
  dataType          _ = gdtUInt32
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
deriveNullableVector "Word32" [t|Value Word32|] [t|St.Vector|]
deriveNullableVector "DynWord32" [t|Value (DynType Word32)|] [t|GV.Vector|]
dynGType(Word32)

instance GDALType Int8 where
  dataType          _ = gdtByte
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
deriveNullableVector "Int8" [t|Value Int8|] [t|St.Vector|]
deriveNullableVector "DynInt8" [t|Value (DynType Int8)|] [t|GV.Vector|]
dynGType(Int8)

instance GDALType Int16 where
  dataType          _ = gdtInt16
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
deriveNullableVector "Int16" [t|Value Int16|] [t|St.Vector|]
deriveNullableVector "DynInt16" [t|Value (DynType Int16)|] [t|GV.Vector|]
dynGType(Int16)

instance GDALType Int32 where
  dataType          _ = gdtInt32
  gToIntegral         = fromIntegral
  gFromIntegral       = fromIntegral
  gToReal             = fromIntegral
  gFromReal           = truncate
  gToIntegralPair   v = Pair (fromIntegral v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (fromIntegral v, 0)
  gFromRealPair       = truncate . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
deriveNullableVector "Int32" [t|Value Int32|] [t|St.Vector|]
deriveNullableVector "DynInt32" [t|Value (DynType Int32)|] [t|GV.Vector|]
dynGType(Int32)

instance GDALType Float where
  dataType          _ = gdtFloat32
  gToIntegral         = truncate
  gFromIntegral       = fromIntegral
  gToReal             = realToFrac
  gFromReal           = realToFrac
  gToIntegralPair   v = Pair (truncate v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (realToFrac v, 0)
  gFromRealPair       = realToFrac . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
deriveNullableVector "Float" [t|Value Float|] [t|St.Vector|]
deriveNullableVector "DynFloat" [t|Value (DynType Float)|] [t|GV.Vector|]
dynGType(Float)

instance GDALType Double where
  dataType          _ = gdtFloat64
  gToIntegral         = truncate
  gFromIntegral       = fromIntegral
  gToReal             = realToFrac
  gFromReal           = realToFrac
  gToIntegralPair   v = Pair (truncate v, 0)
  gFromIntegralPair   = fromIntegral . fst . unPair
  gToRealPair       v = Pair (realToFrac v, 0)
  gFromRealPair       = realToFrac . fst . unPair
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
deriveNullableVector "Double" [t|Value Double|] [t|St.Vector|]
deriveNullableVector "DynDouble" [t|Value (DynType Double)|] [t|GV.Vector|]
dynGType(Double)

instance GDALType (Complex Int16) where
  dataType _          = gdtCInt16
  gToIntegral         = fromIntegral . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = fromIntegral . realPart
  gFromReal         v = truncate v :+ 0
  gToIntegralPair     = fmap fromIntegral . Pair . (realPart &&& imagPart)
  gFromIntegralPair   = uncurry (:+) . unPair . fmap fromIntegral
  gToRealPair         = fmap fromIntegral . Pair . (realPart &&& imagPart)
  gFromRealPair       = uncurry (:+) . unPair . fmap truncate
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
#if MIN_VERSION_base(4,8,0)
deriveNullableVector "ComplexInt16" [t|Value (Complex Int16)|] [t|St.Vector|]
#else
deriveNullableVector "ComplexInt16" [t|Value (Complex Int16)|] [t|GV.Vector|]
#endif
deriveNullableVector
  "DynCInt16" [t|Value (DynType (Complex Int16))|] [t|GV.Vector|]
dynGType(Complex Int16)



instance GDALType (Complex Int32) where
  dataType          _ = gdtCInt32
  gToIntegral         = fromIntegral . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = fromIntegral . realPart
  gFromReal         v = truncate v :+ 0
  gToIntegralPair     = fmap fromIntegral . Pair . (realPart &&& imagPart)
  gFromIntegralPair   = uncurry (:+) . unPair . fmap fromIntegral
  gToRealPair         = fmap fromIntegral . Pair . (realPart &&& imagPart)
  gFromRealPair       = uncurry (:+) . unPair . fmap truncate
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
#if MIN_VERSION_base(4,8,0)
deriveNullableVector "ComplexInt32" [t|Value (Complex Int32)|] [t|St.Vector|]
#else
deriveNullableVector "ComplexInt32" [t|Value (Complex Int32)|] [t|GV.Vector|]
#endif
deriveNullableVector
  "DynCInt32" [t|Value (DynType (Complex Int32))|] [t|GV.Vector|]
dynGType(Complex Int32)

instance GDALType (Complex Float) where
  dataType          _ = gdtCFloat32
  gToIntegral         = truncate . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = realToFrac . realPart
  gFromReal         v = realToFrac v :+ 0
  gToIntegralPair     = fmap truncate . Pair . (realPart &&& imagPart)
  gFromIntegralPair   = uncurry (:+) . unPair . fmap fromIntegral
  gToRealPair         = fmap realToFrac . Pair . (realPart &&& imagPart)
  gFromRealPair       = uncurry (:+) . unPair . fmap realToFrac
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
#if MIN_VERSION_base(4,8,0)
deriveNullableVector "ComplexFloat" [t|Value (Complex Float)|] [t|St.Vector|]
#else
deriveNullableVector "ComplexFloat" [t|Value (Complex Float)|] [t|GV.Vector|]
#endif
deriveNullableVector
  "DynCFloat" [t|Value (DynType (Complex Float))|] [t|GV.Vector|]
dynGType(Complex Float)

instance GDALType (Complex Double) where
  dataType          _ = gdtCFloat64
  gToIntegral         = truncate . realPart
  gFromIntegral     v = fromIntegral v :+ 0
  gToReal             = realToFrac . realPart
  gFromReal         v = realToFrac v :+ 0
  gToIntegralPair     = fmap truncate . Pair . (realPart &&& imagPart)
  gFromIntegralPair   = uncurry (:+) . unPair . fmap fromIntegral
  gToRealPair         = fmap realToFrac . Pair . (realPart &&& imagPart)
  gFromRealPair       = uncurry (:+) . unPair . fmap realToFrac
  {-# INLINE dataType #-}
  {-# INLINE gToIntegral       #-}
  {-# INLINE gFromIntegral     #-}
  {-# INLINE gToReal           #-}
  {-# INLINE gFromReal         #-}
  {-# INLINE gToIntegralPair   #-}
  {-# INLINE gFromIntegralPair #-}
  {-# INLINE gToRealPair       #-}
  {-# INLINE gFromRealPair     #-}
#if MIN_VERSION_base(4,8,0)
deriveNullableVector "ComplexDouble" [t|Value (Complex Double)|] [t|St.Vector|]
#else
deriveNullableVector "ComplexDouble" [t|Value (Complex Double)|] [t|GV.Vector|]
#endif
deriveNullableVector
  "DynCDouble" [t|Value (DynType (Complex Double))|] [t|GV.Vector|]
dynGType(Complex Double)
