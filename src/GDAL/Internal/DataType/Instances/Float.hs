{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module GDAL.Internal.DataType.Instances.Float () where

import GDAL.Internal.Types.Value (Value)
import GDAL.Internal.Types.Vector.Masked (deriveNullableVector)
import qualified GDAL.Internal.Types.Vector as GV
import GDAL.Internal.Types.Pair (Pair(..))
import GDAL.Internal.DataType

import qualified Data.Vector.Storable         as St



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
deriving instance GDALType (DynType Float)
deriveNullableVector "DynFloat" [t|Value (DynType Float)|] [t|GV.Vector|]


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
deriving instance GDALType (DynType Double)
deriveNullableVector "DynDouble" [t|Value (DynType Double)|] [t|GV.Vector|]
