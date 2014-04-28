{-# LANGUAGE CPP, BangPatterns #-}

module Language.Haskell.GhcMod.Find where

import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Maybe (fromMaybe)
import GHC (Ghc)
import qualified GHC as G
import Language.Haskell.GhcMod.Browse (browseAll)
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif

#if MIN_VERSION_containers(0,5,0)
import Control.DeepSeq (force)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
#else
import Data.Map (Map)
import qualified Data.Map as M
#endif
import Control.Applicative ((<$>))

-- | Type of key for `SymMdlDb`.
type Symbol = String
-- | Database from 'Symbol' to modules.
newtype SymMdlDb = SymMdlDb (Map Symbol [ModuleString])

-- | Find modules to which the symbol belong.
findSymbol :: Options -> Cradle -> Symbol -> IO String
findSymbol opt cradle sym = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    lookupSym opt sym <$> getSymMdlDb

-- | Creating 'SymMdlDb'.
getSymMdlDb :: Ghc SymMdlDb
getSymMdlDb = do
    sm <- G.getSessionDynFlags >>= browseAll
#if MIN_VERSION_containers(0,5,0)
    let !sms = force $ map tieup $ groupBy ((==) `on` fst) $ sort sm
        !m = force $ M.fromList sms
#else
    let !sms = map tieup $ groupBy ((==) `on` fst) $ sort sm
        !m = M.fromList sms
#endif
    return (SymMdlDb m)
  where
    tieup x = (head (map fst x), map snd x)

-- | Looking up 'SymMdlDb' with 'Symbol' to find modules.
lookupSym :: Options -> Symbol -> SymMdlDb -> String
lookupSym opt sym (SymMdlDb db) = convert opt $ fromMaybe [] (M.lookup sym db)
