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

module GDAL.Internal.DataType.Instances.Int () where

import GDAL.Internal.Types.Value (Value)
import GDAL.Internal.Types.Vector.Masked (deriveNullableVector)
import qualified GDAL.Internal.Types.Vector as GV
import GDAL.Internal.Types.Pair (Pair(..))
import GDAL.Internal.DataType

import qualified Data.Vector.Storable         as St
import Data.Int (Int8, Int16, Int32)

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
deriving instance GDALType (DynType Int8)
deriveNullableVector "DynInt8" [t|Value (DynType Int8)|] [t|GV.Vector|]


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
deriving instance GDALType (DynType Int16)
deriveNullableVector "DynInt16" [t|Value (DynType Int16)|] [t|GV.Vector|]


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
deriving instance GDALType (DynType Int32)
deriveNullableVector "DynInt32" [t|Value (DynType Int32)|] [t|GV.Vector|]
