{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module GDAL.Internal.Util (
    fromEnumC
  , toEnumC
  , createEnum
  , runBounded
) where

import Control.Concurrent (runInBoundThread, rtsSupportsBoundThreads)
import Foreign.C.Types (CInt)
import Language.Haskell.TH

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum
{-# INLINE fromEnumC #-}

toEnumC :: Enum a => CInt -> a
toEnumC = toEnum . fromIntegral
{-# INLINE toEnumC #-}

createEnum :: String -> IO [String] -> Q [Dec]
createEnum name getNames = do
  names <- runIO getNames
  let ctors = map (\n -> NormalC (mkName n) []) names
  return $ [DataD [] (mkName name) [] ctors [''Show, ''Enum, ''Eq, ''Read]]

runBounded :: IO a -> IO a
runBounded
  | rtsSupportsBoundThreads = runInBoundThread
  | otherwise               = id
{-# INLINE runBounded #-}
