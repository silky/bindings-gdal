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
{-# LANGUAGE StandaloneDeriving #-}

module GDAL.Internal.DataType.Instances.Complex () where

import GDAL.Internal.Types.Value (Value)
import GDAL.Internal.Types.Pair (Pair(..))
import GDAL.Internal.Types.Vector.Masked (deriveNullableVector)
import qualified GDAL.Internal.Types.Vector as GV
import GDAL.Internal.DataType

import Control.Arrow ((&&&))
import Data.Int (Int16, Int32)
import qualified Data.Vector.Storable         as St


#if MIN_VERSION_base(4,8,0)
import Data.Complex (Complex((:+)), realPart, imagPart)
#else
import Data.Complex (Complex((:+)))
import qualified GDAL.Internal.Types.Vector as GV
realPart, imagPart :: Complex t -> t
realPart (a :+ _) = a
imagPart (_ :+ a) = a
#endif


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
deriving instance GDALType (DynType (Complex Int16))



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
deriving instance GDALType (DynType (Complex Int32))


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
deriving instance GDALType (DynType (Complex Float))


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
deriving instance GDALType (DynType (Complex Double))
