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

module GDAL.Internal.DataType.Instances.Word () where

import GDAL.Internal.Types.Value (Value)
import GDAL.Internal.Types.Vector.Masked (deriveNullableVector)
import qualified GDAL.Internal.Types.Vector as GV
import GDAL.Internal.Types.Pair (Pair(..))
import GDAL.Internal.DataType

import qualified Data.Vector.Storable         as St
import Data.Word (Word8, Word16, Word32)


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
deriving instance GDALType (DynType Word8)




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
deriving instance GDALType (DynType Word16)

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
deriving instance GDALType (DynType Word32)
