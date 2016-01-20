{-# LANGUAGE CPP, TemplateHaskell #-}
-- | This module uses scope lookup techniques to either export
-- 'lookupValueName' from @Language.Haskell.TH@, or define
-- its own 'lookupValueName', which attempts to do the
-- same job with just 'reify'. This will sometimes fail, but if it
-- succeeds it will give the answer that the real function would have
-- given.
--
-- The idea is that if you use lookupValueName from this module,
-- your client code will automatically use the best available name
-- lookup mechanism. This means that e.g. 'scopeLookup' can work
-- very well on recent GHCs and less well but still somewhat
-- usefully on older GHCs.
module NotCPP.LookupValueName (
  lookupValueName
 ) where

import Language.Haskell.TH

import NotCPP.Utils

bestValueGuess :: String -> Q (Maybe Name)
bestValueGuess s = do
  mi <- maybeReify (mkName s)
  case mi of
    Nothing -> no
    Just i -> case i of
#if __GLASGOW_HASKELL__ >= 800
      VarI n _ _ -> yes n
      DataConI n _ _ -> yes n
#else
      VarI n _ _ _ -> yes n
      DataConI n _ _ _ -> yes n
#endif
      _ -> err ["unexpected info:", show i]
 where
  no = return Nothing
  yes = return . Just
  err = fail . showString "NotCPP.bestValueGuess: " . unwords

$(recover [d| lookupValueName = bestValueGuess |] $ do
#if __GLASGOW_HASKELL__ >= 800
  VarI _ _ _ <- reify (mkName "lookupValueName")
#else
  VarI _ _ _ _ <- reify (mkName "lookupValueName")
#endif
  return [])
