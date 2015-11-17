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
import GDAL.Internal.Types.Pair
import qualified GDAL.Internal.Vector.Translated  as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Arrow
import Control.Monad
import Data.List
import Data.Function
import Language.Haskell.TH

import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as M

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

{-
  let mvName = mkName ("MV_" ++ name)
      vName = mkName ("V_" ++ name)
      mvCon = ConE mvName
      vCon  = ConE vName

  i <- newPatExp "idx"
  n <- newPatExp "len"
  mv  <- first (ConP mvName . (:[])) <$> newPatExp "mvec"
  mv' <- first (ConP mvName . (:[])) <$> newPatExp "mvec'"
  v   <- first (ConP vName  . (:[])) <$> newPatExp "vec"
  a <- newPatExp "val"

  let mvClauses = [
          wrapperClause 'M.basicLength           [mv]        id
        , wrapperClause 'M.basicUnsafeSlice      [i, n, mv]  (AppE mvCon)
        , wrapperClause 'M.basicOverlaps         [mv, mv']   id
        , ('M.basicUnsafeNew,
             Clause [] (NormalB (VarE 'newMVector `AppE` gtype)) [])
#if MIN_VERSION_vector(0,11,0)
        , wrapperClause 'M.basicInitialize       [mv]        id
#endif
        , wrapperClause 'M.basicUnsafeReplicate  [n, a]      (liftE mvCon)
        , wrapperClause 'M.basicUnsafeRead       [mv, i]     id
        , wrapperClause 'M.basicUnsafeWrite      [mv, i, a]  id
        , wrapperClause 'M.basicClear            [mv]        id
        , wrapperClause 'M.basicSet              [mv, a]     id
        , wrapperClause 'M.basicUnsafeCopy       [mv, mv']   id
        , wrapperClause 'M.basicUnsafeMove       [mv, mv']   id
        , wrapperClause 'M.basicUnsafeGrow       [mv, n]     (liftE mvCon)
        ]

  let vClauses = [
          wrapperClause 'G.basicUnsafeFreeze     [mv]        (liftE vCon)
        , wrapperClause 'G.basicUnsafeThaw       [v]         (liftE mvCon)
        , wrapperClause 'G.basicLength           [v]         id
        , wrapperClause 'G.basicUnsafeSlice      [i, n, v]   (AppE vCon)
        , wrapperClause 'G.basicUnsafeIndexM     [v, i]      id
        , wrapperClause 'G.basicUnsafeCopy       [mv, v]     id
        , wrapperClause 'G.elemseq               [v, a]      id
        ]


  let vType = ConT ''G.Vector `AppT` ConT ''Vector `AppT` type_
      instanceVector = InstanceD [] vType (mkMultiClauseFuns vClauses)

      mvType = ConT ''M.MVector `AppT` ConT ''MVector `AppT` type_
      instanceMVector = InstanceD [] mvType (mkMultiClauseFuns mvClauses)

  s <- VarT <$> newName "s"
  let dataMVector =
        NewtypeInstD [] ''MVector [s, type_]
        (NormalC mvName
          [(NotStrict, ConT ''T.MVector `AppT` s `AppT` type_)]) []
      dataVector =
        NewtypeInstD [] ''Vector [type_]
        (NormalC vName
          [(NotStrict, ConT ''T.Vector `AppT` type_)]) []

-}

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


  let gInstClauses = concat arrayClauses ++ [
          ('dataType, Clause  [WildP] (NormalB (gtype)) [])
        {-
        , ('unsafeAsNative,
            Clause
              [ConP vName [VarP tv]]
              (NormalB (VarE 'T.unsafeAsNative `AppE` VarE tv)) [])
        , ('unsafeAsNativeM,
            Clause
              [ConP mvName [VarP tv]]
              (NormalB (VarE 'T.unsafeAsNativeM `AppE` VarE tv)) [])
        , ('unsafeAsDataType,
            Clause
              [VarP dt, ConP vName [VarP tv]]
              (NormalB (VarE 'T.unsafeAsDataType `AppE` VarE dt `AppE` VarE tv)) [])
        , ('newMVector,
            Clause
              [VarP dt, VarP j]
              (NormalB (ConE mvName `liftE`
                          (VarE 'T.newMVector `AppE` VarE dt `AppE` VarE j)
                       )) [])
        -}
        ]

  let instanceGDALType =
        InstanceD []
         (ConT ''GDALType `AppT` type_)
         (mkMultiClauseFuns gInstClauses)
  return
    [ instanceGDALType
    -- , dataMVector, instanceMVector
    -- , dataVector, instanceVector
    ]



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
