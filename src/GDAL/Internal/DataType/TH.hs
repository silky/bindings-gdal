{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GDAL.Internal.DataType.TH (deriveGDALType) where

import GDAL.Internal.DataType
import qualified GDAL.Internal.DataType as DT
import GDAL.Internal.Types.Pair
import GDAL.Internal.Types.Value

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Arrow
import Control.Monad
import Data.List
import Data.Function
import Language.Haskell.TH

import Data.Int (Int8, Int16, Int32)
import Data.Primitive.Types
import Data.Word (Word8, Word16, Word32)

import GHC.Prim
import GHC.Base
import GHC.Word
import GHC.Int

------------------------------------------------------------------------------
-- instance deriver
------------------------------------------------------------------------------
deriveGDALType
  :: String
  -> Q Type
  -> Q Exp
  -> Q Exp
  -> Q Exp
  -> Q Exp
  -> Q Exp
  -> Q Exp
  -> Q Exp
  -> Q Exp
  -> Q Exp
  -> Q [Dec]
deriveGDALType name typeQ gtypeQ toInt fromInt toReal fromReal toInt2 fromInt2
                                 toReal2 fromReal2 = do
  type_ <- typeQ
  gtype <- gtypeQ

  let tName = case type_ of
        (ConT _ `AppT` ConT n) -> nameBase n
        (ConT n)               -> nameBase n
        _                      -> name

  let pTypes' = [ (False, False, ''Word8  , 'W8#  , GDT_Byte    )
                , (False, False, ''Word16 , 'W16# , GDT_UInt16  )
                , (False, False, ''Word32 , 'W32# , GDT_UInt32  )
                , (False, False, ''Int16  , 'I16# , GDT_Int16   )
                , (False, False, ''Int32  , 'I32# , GDT_Int32   )
                , (False, True , ''Float  , 'F#   , GDT_Float32 )
                , (False, True , ''Double , 'D#   , GDT_Float64 )
                , (True , False, ''Int16  , 'I16# , GDT_CInt16  )
                , (True , False, ''Int32  , 'I32# , GDT_CInt32  )
                , (True , True , ''Float  , 'F#   , GDT_CFloat32)
                , (True , True , ''Double , 'D#   , GDT_CFloat64)
                ]
      pTypes = sortBy (\a b -> if a/=b && pTy a == tName then LT else GT)
                      pTypes'
      pTy (_,_,t,_,_) = nameBase t

  arrayClauses <- forM pTypes $ \(isPair, isReal, primName, pConName, dt) -> do
    let (toRepQ, fromRepQ) =
          case (isPair, isReal) of
               (False , False ) -> (toInt, fromInt)
               (False , True  ) -> (toReal, fromReal)
               (True  , False ) -> (toInt2, fromInt2)
               (True  , True  ) -> (toReal2, fromReal2)
        nStr = nameBase primName ++ "Array#"
        readF = return (VarE (mkName ("GHC.Prim.read" ++ nStr)))
        writeF = return (VarE (mkName ("GHC.Prim.write" ++ nStr)))
        indexF = return (VarE (mkName ("GHC.Prim.index" ++ nStr)))
        pConE = return (ConE pConName)
        pat   = LitP (IntPrimL (fromIntegral (fromEnum dt)))
        funs i r w = [
            ('gIndexByteArray#, (Clause [pat] (NormalB i) []))
          , ('gReadByteArray#, (Clause [pat] (NormalB r) []))
          , ('gWriteByteArray#, (Clause [pat] (NormalB w) []))
          ]

    if isPair
      then do
        read# <- [|\a# i# s# ->
                   case $(readF) a# (i# *# 2#) s# of {(# s1#, v1# #) ->
                   case $(readF) a# (i# *# 2# +# 1#) s1# of {(# s2#, v2# #) ->
                   (# s2#, $(fromRepQ) (Pair ($pConE v1#, $pConE v2#)) #)}}|]
        index# <- [|\a# i# -> $fromRepQ (
                    Pair ( $pConE ($indexF a# (i# *# 2#))
                         , $pConE ($indexF a# (i# *# 2# +# 1#)))) |]
        (pQ1, vQ1) <- newPatConExpQ pConName "v1"
        (pQ2, vQ2) <- newPatConExpQ pConName "v2"
        write# <- [|\a# i# v s# ->
                    case $toRepQ v of {Pair ($pQ1, $pQ2) ->
                    case $writeF a# (i# *# 2#) $vQ1 s# of { s1# ->
                    $writeF a# (i# *# 2# +# 1#) $vQ2 s1#}}|]

        return (funs index# read# write#)
      else do
        read# <- [|\a# i# s# ->
                    case $readF a# i# s# of
                      (# s1#, v# #) -> (# s1#, $fromRepQ ($pConE v#) #) |]
        index# <- [|\a# i# ->
                    case $indexF a# i# of
                      v# -> $fromRepQ ($pConE v#) |]
        (pQ, vQ) <- newPatConExpQ pConName "v"
        write# <- [|\a# i# v s# ->
                     case $toRepQ v of
                       $pQ -> $writeF a# i# $vQ s# |]
        return (funs index# read# write#)


  tv <- newName "tv"
  j <- newName "j"
  dt <- newName "dt"

  toDouble' <- toReal
  fromDouble' <- fromReal

  let gInstClauses = concat arrayClauses ++ [
          ('dataType, Clause  [WildP] (NormalB (gtype)) [])
        , ('toDouble, Clause  [] (NormalB (toDouble')) [])
        , ('fromDouble, Clause  [] (NormalB (fromDouble')) [])
        ]

  let instanceGDALType =
        InstanceD []
         (ConT ''GDALType `AppT` type_)
         (mkMultiClauseFuns gInstClauses)
  return
    [ instanceGDALType
    ]


wrapDoubleConv :: (a -> b) -> a -> b
wrapDoubleConv = id
{-# INLINE[0] wrapDoubleConv #-}

-- Create a @Pat@ bound to the given name and an @Exp@ for said binding.
newPatExp :: String -> Q (Pat, Exp)
newPatExp = fmap (VarP &&& VarE) . newName

newPatConExpQ :: Name -> String -> Q (Q Pat, Q Exp)
newPatConExpQ cName =
  fmap ((return . (\v -> ConP cName [VarP v])) &&& (return . VarE)) . newName

liftE :: Exp -> Exp -> Exp
liftE e = InfixE (Just e) (VarE 'liftM) . Just


mkMultiClauseFuns :: [(Name,Clause)] -> [Dec]
mkMultiClauseFuns nameClauses =
  concat [ [ inlinePragma (fst (head fun_clauses))
           , FunD (fst (head fun_clauses)) (map snd fun_clauses)
           ] | fun_clauses <- groupBy ((==) `on` fst) $
                              sortBy (compare `on` fst) $ nameClauses]

wrapperClause :: Name -> [(Pat, Exp)] -> (Exp -> Exp) -> (Name, Clause)
wrapperClause fun (unzip -> (pats, exps)) coerce =
  (fun, Clause pats (NormalB body) [])
  where body = coerce $ foldl AppE (VarE fun) exps

#if MIN_VERSION_template_haskell(2,8,0)
inlinePragma name = PragmaD (InlineP name Inline FunLike AllPhases)
#else
inlinePragma name = PragmaD ( InlineP name (InlineSpec True False Nothing) )
#endif
