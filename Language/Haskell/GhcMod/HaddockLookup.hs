{-# LANGUAGE CPP                    #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}

module Language.Haskell.GhcMod.HaddockLookup (haddock) where

import Exception (ghandle)
-- import Control.Exception (SomeException(..))
-- import Language.Haskell.GhcMod.Logger (checkErrorPrefix)
-- import Language.Haskell.GhcMod.Convert
-- import Language.Haskell.GhcMod.Types
-- import Language.Haskell.GhcMod.Monad
-- import Language.Haskell.HLint (hlint)

-- import Language.Haskell.GhcMod.Utils (withMappedFile)

-- import Data.List (stripPrefix)

import Data.IORef
import Control.Applicative
import Control.Monad
import Data.Char (isAlpha)
import Data.List
import Data.Maybe
import Data.Typeable()
import Desugar()
import FastString
import GHC
import GHC.Paths (libdir)
import GHC.SYB.Utils()
import HscTypes
import Outputable
import RdrName
import System.Directory
import System.Environment()
import System.FilePath
import System.IO
import System.Process
import TcRnTypes()
import System.Process.Streaming
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal (w2c)

import qualified GhcMonad
import qualified MonadUtils()
import qualified Packages
import qualified SrcLoc
import qualified Safe
import qualified GHC as G

import qualified Data.Map as M

import Language.Haskell.GhcMod.Monad ( runGmOutT )
import qualified Language.Haskell.GhcMod.Types as GhcModTypes

import Language.Haskell.GhcMod.Types       (IOish)
import Language.Haskell.GhcMod.Monad.Types (GhcModLog(..), GmOut(..))
import Control.Monad.Trans.Journal (runJournalT)

-- import Language.Haskell.GhcImportedFrom.UtilsFromGhcMod
-- import Language.Haskell.GhcImportedFrom.Types

import Control.Exception (SomeException)

import qualified Text.Parsec as TP
import Data.Functor.Identity

import qualified Documentation.Haddock as Haddock

import Control.Exception
import Control.Monad.Catch

import qualified DynFlags()

import Language.Haskell.GhcMod

import Language.Haskell.GhcMod.SrcUtils (listifySpans)
import Language.Haskell.GhcMod.Pretty

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Doc
import Language.Haskell.GhcMod.FileMapping
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.PkgDoc

import Data.List

#if __GLASGOW_HASKELL__ >= 708
import DynFlags ( unsafeGlobalDynFlags )
tdflags :: DynFlags
tdflags = unsafeGlobalDynFlags
#else
import DynFlags ( tracingDynFlags )
tdflags :: DynFlags
tdflags = tracingDynFlags
#endif

-- FIXME We don't support LineSeparator; might be handy for
-- Windows (?) with CRLF encoding?
-- newtype LineSeparator = LineSeparator String deriving (Show)

data FixmeOptions = FixmeOptions {
      ghcOpts       :: [String]
    , ghcPkgOpts    :: [String]
    , lineSeparator :: LineSeparator
    } deriving (Show)

defaultFixmeOptions :: FixmeOptions
defaultFixmeOptions = FixmeOptions {
      ghcOpts       = []
    , ghcPkgOpts    = []
    , lineSeparator = LineSeparator "\0"
    }


type GHCFixmeOption = String

type QualifiedName = String -- ^ A qualified name, e.g. @Foo.bar@.

type FixmeSymbol = String -- ^ A symbol, possibly qualified, e.g. @bar@ or @Foo.bar@.

newtype GhcFixmeOptions
    -- | List of user-supplied GHC options, refer to @tets@ subdirectory for example usage. Note that
    -- GHC API and ghc-pkg have inconsistencies in the naming of options, see <http://www.vex.net/~trebla/haskell/sicp.xhtml> for more details.
    = GhcFixmeOptions [String] deriving (Show)

--instance Monoid GhcOptions where
--    mempty  = GhcOptions []
--    (GhcOptions g) `mappend` (GhcOptions h) = GhcOptions $ g ++ h

newtype GhcPkgFixmeOptions
    -- | List of user-supplied ghc-pkg options.
    = GhcPkgFixmeOptions [String] deriving (Show)

data HaskellModule
    -- | Information about an import of a Haskell module.
    = HaskellModule { modName           :: String
                    , modQualifier      :: Maybe String
                    , modIsImplicit     :: Bool
                    , modHiding         :: [String]
                    , modImportedAs     :: Maybe String
                    , modSpecifically   :: [String]
                    } deriving (Show, Eq)


-- trace' :: Show x => String -> x -> b -> b
-- trace' m x = trace (m ++ ">>> " ++ show x)

-- trace'' :: Outputable x => String -> x -> b -> b
-- trace'' m x = trace (m ++ ">>> " ++ showSDoc tdflags (ppr x))

-- | Evaluate IO actions in sequence, returning the first that
-- succeeds.
shortcut :: [IO (Maybe a)] -> IO (Maybe a)
shortcut []     = return Nothing
shortcut (a:as) = do
    a' <- a

    case a' of
        a''@(Just _)    -> return a''
        Nothing         -> shortcut as

executeFallibly' :: String -> [String] -> IO (Maybe (String, String))
executeFallibly' cmd args = do
    x <- (executeFallibly (piped (proc cmd args)) ((,) <$> (foldOut intoLazyBytes) <*> (foldErr intoLazyBytes)))
         `catchIOError` -- FIXME Later, propagate the error so we can log it. Top level type should be an Either or something, not a Maybe.
         (\e -> return $ Left $ show e)

    return $ case x of
        Left e              -> Nothing
        Right (a, b)   -> Just $ (b2s a, b2s b)

  where

    b2s = map w2c . B.unpack . BL.toStrict

-- | Use "stack path" to get the snapshot package db location.
getStackSnapshotPkgDb :: IO (Maybe String)
getStackSnapshotPkgDb = do
    putStrLn "getStackSnapshotPkgDb ..."

    x <- join <$> (fmap (fmap unwords . fmap words . Safe.headMay . lines) . fmap fst) <$> executeFallibly' "stack" ["path", "--snapshot-pkg-db"]

    return $ case x of
        Nothing     -> Nothing
        Just ""     -> Nothing
        Just x'     -> Just x'

-- | Use "stack path" to get the local package db location.
getStackLocalPkgDb :: IO (Maybe String)
getStackLocalPkgDb = do
    putStrLn "getStackLocalPkgDb ..."

    x <- join <$> (fmap (fmap unwords . fmap words . Safe.headMay . lines) . fmap fst) <$> executeFallibly' "stack" ["path", "--local-pkg-db"]

    return $ case x of
        Nothing     -> Nothing
        Just ""     -> Nothing
        Just x'     -> Just x'

-- | GHC options that we don't use when partially compiling the source module.
filterOpts :: [String] -> [String]
filterOpts xs = filter (\x -> x /= "--interactive" && x /= "-fbuilding-cabal-package" && x /= "-Wall") $ dropModuleNames xs

   where

    dropModuleNames :: [String] -> [String]
    dropModuleNames = filter parseHelper

    parseHelper :: String -> Bool
    parseHelper s = case TP.parse (parseFullHaskellModuleName <* TP.eof) "" s of Right _ -> False
                                                                                 Left _  -> True

    parseFullHaskellModuleName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parseFullHaskellModuleName = do
        h <- parseHaskellModuleName
        rest <- many parseDottedHaskellModuleName

        return $ intercalate "." (h:rest)

    parseHaskellModuleName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parseHaskellModuleName = do
        c <- TP.upper
        cs <- TP.many (TP.choice [TP.lower, TP.upper, TP.char '_', TP.digit])
        return (c:cs)

    parseDottedHaskellModuleName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parseDottedHaskellModuleName = TP.char '.' >> parseHaskellModuleName


parsePackageAndQualName :: forall u. TP.ParsecT String u Identity (String, String)
parsePackageAndQualName = TP.choice [TP.try parsePackageAndQualNameWithHash, parsePackageAndQualNameNoHash]

  where

    -- Package with no hash (seems to be for internal packages?)
    -- base-4.8.2.0:Data.Foldable.length
    parsePackageAndQualNameNoHash :: TP.ParsecT String u Data.Functor.Identity.Identity (String, String)
    parsePackageAndQualNameNoHash = do
        packageName <- parsePackageName
        qName       <- parsePackageFinalQualName

        return (packageName, qName)

    parsePackageName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageName = TP.anyChar `TP.manyTill` TP.char ':'

    parsePackageFinalQualName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageFinalQualName = TP.many1 TP.anyChar

-- Parse the package name "containers-0.5.6.2" from a string like
-- "containers-0.5.6.2@conta_2C3ZI8RgPO2LBMidXKTvIU:Data.Map.Base.fromList"
parsePackageAndQualNameWithHash :: TP.ParsecT String u Data.Functor.Identity.Identity (String, String)
parsePackageAndQualNameWithHash = do
    packageName <- parsePackageName
    _           <- parsePackageHash
    qName       <- parsePackageFinalQualName

    return (packageName, qName)

  where

    parsePackageName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageName = TP.anyChar `TP.manyTill` TP.char '@'

    parsePackageHash :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageHash = TP.anyChar `TP.manyTill` TP.char ':'

    parsePackageFinalQualName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageFinalQualName = TP.many1 TP.anyChar

-- |Convenience function for converting an 'GHC.ImportDecl' to a 'HaskellModule'.
--
-- Example:
--
-- > -- Hiding.hs
-- > module Hiding where
-- > import Data.List hiding (map)
-- > import System.Environment (getArgs)
-- > import qualified Safe
--
-- then:
--
-- >>> map toHaskellModule <$> getTextualImports "tests/data/data/Hiding.hs" "Hiding" >>= print
-- [ HaskellModule { modName = "Prelude"
--                 , modQualifier = Nothing
--                 , modIsImplicit = True
--                 , modHiding = []
--                 , modImportedAs = Nothing
--                 , modSpecifically = []
--                 }
-- , HaskellModule {modName = "Safe"
--                 , modQualifier = Nothing
--                 , modIsImplicit = False
--                 , modHiding = []
--                 , modImportedAs = Nothing
--                 , modSpecifically = []
--                 }
-- , HaskellModule { modName = "System.Environment"
--                 , modQualifier = Nothing
--                 , modIsImplicit = False
--                 , modHiding = []
--                 , modImportedAs = Nothing
--                 , modSpecifically = ["getArgs"]
--                 }
-- , HaskellModule { modName = "Data.List"
--                 , modQualifier = Nothing
--                 , modIsImplicit = False
--                 , modHiding = ["map"]
--                 , modImportedAs = Nothing
--                 , modSpecifically = []
--                 }
-- ]
toHaskellModule :: SrcLoc.Located (GHC.ImportDecl GHC.RdrName) -> HaskellModule
toHaskellModule idecl = HaskellModule name qualifier isImplicit hiding importedAs specifically
    where idecl'     = SrcLoc.unLoc idecl
          name       = showSDoc tdflags (ppr $ GHC.ideclName idecl')
          isImplicit = GHC.ideclImplicit idecl'
          qualifier  = unpackFS <$> GHC.ideclPkgQual idecl'
          hiding     = (catMaybes . parseHiding . GHC.ideclHiding) idecl'
          importedAs = (showSDoc tdflags . ppr) <$> ideclAs idecl'
          specifically = (parseSpecifically . GHC.ideclHiding) idecl'

          --grabNames :: GHC.Located (GHC.IE GHC.RdrName) -> String
          --grabNames loc = showSDoc tdflags (ppr names)
          --  where names = GHC.ieNames $ SrcLoc.unLoc loc

          grabNames' :: GHC.Located [GHC.LIE GHC.RdrName] -> [String]
          grabNames' loc = map (showSDoc tdflags . ppr) names
            where names :: [RdrName]
                  names = map (ieName . SrcLoc.unLoc) $ SrcLoc.unLoc loc
                  -- FIXME We are throwing away location info by using unLoc each time?
                  -- Trace these things to see what we are losing.
                  --
          parseHiding :: Maybe (Bool, Located [LIE RdrName]) -> [Maybe String]
          parseHiding Nothing = [Nothing]

          -- If we do
          --
          --     import System.Environment ( getArgs )
          --
          -- then we get ["getArgs"] here, but we don't really need it...
          parseHiding (Just (False, _)) = []

          -- Actually hid names, e.g.
          --
          --     import Data.List hiding (map)
          parseHiding (Just (True, h))  = map Just $ grabNames' h

          parseSpecifically :: Maybe (Bool, Located [LIE RdrName]) -> [String]
          parseSpecifically (Just (False, h)) = grabNames' h
          parseSpecifically _                 = []

-- | List of possible modules which have resulted in
-- the name being in the current scope. Using a
-- global reader we get the provenance data and then
-- get the list of import specs.
symbolImportedFrom :: GlobalRdrElt -> [ModuleName]
symbolImportedFrom occNameLookup = map importSpecModule whys
  where prov = gre_prov occNameLookup :: Provenance
        Imported (whys :: [ImportSpec])  = prov

-- This definition of separateBy is taken
-- from: http://stackoverflow.com/a/4978733
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep' where
  sep' [] = Nothing
  sep' l  = Just . fmap (drop 1) . break (==chr) $ l

-- | Returns True if the 'Symbol' matches the end of the 'QualifiedName'.
--
-- Example:
--
-- >>> postfixMatch "bar" "Foo.bar"
-- True
-- >>> postfixMatch "bar" "Foo.baz"
-- False
-- >>> postfixMatch "bar" "bar"
-- True
postfixMatch :: FixmeSymbol -> QualifiedName -> Bool
postfixMatch originalSymbol qName = endTerm `isSuffixOf` qName
  where endTerm = last $ separateBy '.' originalSymbol

-- | Get the module part of a qualified name.
--
-- Example:
--
-- >>> moduleOfQualifiedName "Foo.bar"
-- Just "Foo"
-- >>> moduleOfQualifiedName "Foo"
-- Nothing
moduleOfQualifiedName :: QualifiedName -> Maybe String
moduleOfQualifiedName qn = if null bits
                                then Nothing
                                else Just $ intercalate "." bits
  where bits = reverse $ drop 1 $ reverse $ separateBy '.' qn

-- | Find the possible qualified names for the symbol at line/col in the given Haskell file and module.
--
-- Example:
--
-- >>> x <- qualifiedName "tests/data/data/Muddle.hs" "Muddle" 27 5 ["Data.Maybe", "Data.List", "Data.Map", "Safe"]
-- >>> forM_ x print
-- "AbsBinds [] []\n  {Exports: [Muddle.h <= h\n               <>]\n   Exported types: Muddle.h\n                     :: Data.Map.Base.Map GHC.Base.String GHC.Base.String\n                   [LclId]\n   Binds: h = Data.Map.Base.fromList [(\"x\", \"y\")]}"
-- "h = Data.Map.Base.fromList [(\"x\", \"y\")]"
-- "Data.Map.Base.fromList [(\"x\", \"y\")]"
-- "Data.Map.Base.fromList"
qualifiedName :: forall m. GhcMonad m => String -> Int -> Int -> [String] -> m [String]
qualifiedName targetModuleName lineNr colNr importList = do
        -- FIXME Move this context stuff elsewhere?
        setContext (map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList))
           `gcatch` (\(s  :: SourceError)    -> do GhcMonad.liftIO $ putStrLn $ "qualifiedName: setContext failed with a SourceError, trying to continue anyway..." ++ show s
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(g  :: GhcApiError)    -> do GhcMonad.liftIO $ putStrLn $ "qualifiedName: setContext failed with a GhcApiError, trying to continue anyway..." ++ show g
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(se :: SomeException)  -> do GhcMonad.liftIO $ putStrLn $ "qualifiedName: setContext failed with a SomeException, trying to continue anyway..." ++ show se
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)

        modSummary <- getModSummary $ mkModuleName targetModuleName :: m ModSummary
        p <- parseModule modSummary   :: m ParsedModule
        t <- typecheckModule p        :: m TypecheckedModule

        let TypecheckedModule{tm_typechecked_source = tcs} = t
            bs = listifySpans tcs (lineNr, colNr) :: [LHsBind Id]
            es = listifySpans tcs (lineNr, colNr) :: [LHsExpr Id]
            ps = listifySpans tcs (lineNr, colNr) :: [LPat Id]

        let foo x = showSDoc tdflags $ ppr x
            bs' = map foo bs
            es' = map foo es
            ps' = map foo ps

        return $ bs' ++ es' ++ ps'

-- Like qualifiedName but uses 'reallyAlwaysQualify' to show the fully qualified name, e.g.
-- "containers-0.5.6.2@conta_2C3ZI8RgPO2LBMidXKTvIU:Data.Map.Base.fromList" instead of
-- "Data.Map.Base.fromList". Will probably replace qualifiedName once more testing has
-- been done. If this works we can also remove 'ghcPkgFindModule' which uses a shell
-- call to try to find the package name.
qualifiedName'
    :: forall m. (GhcMonad m)
    => String -> Int -> Int -> String -> [String] -> m [String]
qualifiedName' targetModuleName lineNr colNr symbol importList = do
        setContext (map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList))
           `gcatch` (\(s  :: SourceError)    -> do GhcMonad.liftIO $ putStrLn $ "qualifiedName: setContext failed with a SourceError, trying to continue anyway..." ++ show s
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(g  :: GhcApiError)    -> do GhcMonad.liftIO $ putStrLn $ "qualifiedName: setContext failed with a GhcApiError, trying to continue anyway..." ++ show g
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(se :: SomeException)  -> do GhcMonad.liftIO $ putStrLn $ "qualifiedName: setContext failed with a SomeException, trying to continue anyway..." ++ show se
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)

        modSummary <- getModSummary $ mkModuleName targetModuleName :: m ModSummary
        p <- parseModule modSummary   :: m ParsedModule
        t <- typecheckModule p        :: m TypecheckedModule

        let TypecheckedModule{tm_typechecked_source = tcs} = t
            bs = listifySpans tcs (lineNr, colNr) :: [LHsBind Id]
            es = listifySpans tcs (lineNr, colNr) :: [LHsExpr Id]
            ps = listifySpans tcs (lineNr, colNr) :: [LPat Id]
            -- ls0 = listifySpans tcs (lineNr, colNr) :: [LHsBindLR Id Id]
            -- ls1 = listifySpans tcs (lineNr, colNr) :: [LIPBind Id]
            -- ls2 = listifySpans tcs (lineNr, colNr) :: [LPat Id]
            -- ls3 = listifySpans tcs (lineNr, colNr) :: [LHsDecl Id]
            -- ls4 = listifySpans tcs (lineNr, colNr) :: [LHsExpr Id]
            -- ls5 = listifySpans tcs (lineNr, colNr) :: [LHsTupArg Id]
            -- ls6 = listifySpans tcs (lineNr, colNr) :: [LHsCmd Id]
            -- ls7 = listifySpans tcs (lineNr, colNr) :: [LHsCmdTop Id]

        let bs' = map (showSDocForUser tdflags reallyAlwaysQualify . ppr) bs
            es' = map (showSDocForUser tdflags reallyAlwaysQualify . ppr) es
            ps' = map (showSDocForUser tdflags reallyAlwaysQualify . ppr) ps

        return $ filter (postfixMatch symbol) $ concatMap words $ bs' ++ es' ++ ps'

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle h = do
    ineof <- hIsEOF h
    if ineof
        then return ""
        else hGetContents h

optsForGhcPkg :: [String] -> [String]
optsForGhcPkg [] = []
optsForGhcPkg ("-no-user-package-db":rest)   = "--no-user-package-db"          : optsForGhcPkg rest
optsForGhcPkg ("-package-db":pd:rest)        = ("--package-db" ++ "=" ++ pd)   : optsForGhcPkg rest
optsForGhcPkg ("-package-conf":pc:rest)      = ("--package-conf" ++ "=" ++ pc) : optsForGhcPkg rest
optsForGhcPkg ("-no-user-package-conf":rest) = "--no-user-package-conf"        : optsForGhcPkg rest
optsForGhcPkg (_:rest) = optsForGhcPkg rest

ghcPkgFindModule :: {- [String] -> GhcPkgFixmeOptions -> -} String -> IO (Maybe String)
ghcPkgFindModule {- allGhcOptions (GhcPkgFixmeOptions extraGHCPkgOpts) -} m =
    shortcut [ stackGhcPkgFindModule m
             , hcPkgFindModule   m
             , _ghcPkgFindModule {- allGhcOptions (GhcPkgFixmeOptions extraGHCPkgOpts) -} m
             ]

-- | Call @ghc-pkg find-module@ to determine that package that provides a module, e.g. @Prelude@ is defined
-- in @base-4.6.0.1@.
_ghcPkgFindModule :: {- [String] -> GhcPkgFixmeOptions -> -} String -> IO (Maybe String)
_ghcPkgFindModule {- allGhcOptions (GhcPkgFixmeOptions extraGHCPkgOpts) -} m = do
    let opts = ["find-module", m, "--simple-output"] ++ ["--global", "--user"] ++ optsForGhcPkg [] {- allGhcOptions ++ extraGHCPkgOpts -}
    putStrLn $ "ghc-pkg " ++ show opts

    x <- executeFallibly' "ghc-pkg" opts

    case x of
        Nothing             -> return Nothing
        Just (output, err)  -> do putStrLn $ "_ghcPkgFindModule stdout: " ++ show output
                                  putStrLn $ "_ghcPkgFindModule stderr: " ++ show err
                                  return $ join $ (Safe.lastMay . words) <$> (Safe.lastMay . lines) output

-- | Call @cabal sandbox hc-pkg@ to find the package the provides a module.
hcPkgFindModule :: String -> IO (Maybe String)
hcPkgFindModule m = do
    let opts = ["sandbox", "hc-pkg", "find-module", m, "--", "--simple-output"]

    x <- executeFallibly' "cabal" opts

    case x of
        Nothing             -> return Nothing
        Just (output, err)  -> do putStrLn $ "hcPkgFindModule stdout: " ++ show output
                                  putStrLn $ "hcPkgFindModule stderr: " ++ show err
                                  return $ join $ (Safe.lastMay . words) <$> (Safe.lastMay . lines) output

-- | Call @stack exec ghc-pkg@ to find the package the provides a module.
stackGhcPkgFindModule :: String -> IO (Maybe String)
stackGhcPkgFindModule m = do
    let opts = ["exec", "ghc-pkg", "find-module", m, "--", "--simple-output"]

    x <- executeFallibly' "stack" opts

    case x of
        Nothing             -> return Nothing
        Just (output, err)  -> do putStrLn $ "stackGhcPkgFindModule stdout: " ++ show output
                                  putStrLn $ "stackGhcPkgFindModule stderr: " ++ show err
                                  return $ join $ (Safe.lastMay . words) <$> (Safe.lastMay . lines) output

ghcPkgHaddockUrl :: {- [String] -> GhcPkgFixmeOptions -> -} String -> IO (Maybe String)
ghcPkgHaddockUrl {- allGhcOptions (GhcPkgFixmeOptions extraGHCPkgOpts) -} p =
    shortcut [ stackPkgHaddockUrl p
             , sandboxPkgHaddockUrl p
             , _ghcPkgHaddockUrl {- allGhcOptions (GhcPkgFixmeOptions extraGHCPkgOpts) -} p
             ]

-- | Call @ghc-pkg field@ to get the @haddock-html@ field for a package.
_ghcPkgHaddockUrl :: {- [String] -> GhcPkgFixmeOptions -> -} String -> IO (Maybe String)
_ghcPkgHaddockUrl {- allGhcOptions (GhcPkgFixmeOptions extraGHCPkgOpts) -} p = do
    let opts = ["field", p, "haddock-html"] ++ ["--global", "--user"] ++ optsForGhcPkg [] {- allGhcOptions ++ extraGHCPkgOpts -}
    putStrLn $ "ghc-pkg "++ show opts

    x <- executeFallibly' "ghc-pkg" opts

    case x of
        Nothing             -> return Nothing
        Just (hout, _)      -> return $ Safe.lastMay $ words $ reverse . dropWhile (== '\n') . reverse $ hout

readHaddockHtmlOutput :: FilePath -> [String] -> IO (Maybe String)
readHaddockHtmlOutput cmd opts = do
    x <- executeFallibly' cmd opts

    case x of
        Nothing             -> return Nothing
        Just (hout, _)      -> do let line = reverse . dropWhile (== '\n') . reverse $ hout
                                  print ("line", line)

                                  if "haddock-html:" `isInfixOf` line
                                      then do print ("line2", Safe.lastMay $ words line)
                                              return $ Safe.lastMay $ words line
                                      else return Nothing

-- | Call cabal sandbox hc-pkg to find the haddock url.
sandboxPkgHaddockUrl :: String -> IO (Maybe String)
sandboxPkgHaddockUrl p = do
    let opts = ["sandbox", "hc-pkg", "field", p, "haddock-html"]
    putStrLn $ "cabal sandbox hc-pkg field " ++ p ++ " haddock-html"
    readHaddockHtmlOutput "cabal" opts

-- | Call cabal stack to find the haddock url.
stackPkgHaddockUrl :: String -> IO (Maybe String)
stackPkgHaddockUrl p = do
    let opts = ["exec", "ghc-pkg", "field", p, "haddock-html"]
    putStrLn $ "stack exec hc-pkg field " ++ p ++ " haddock-html"
    readHaddockHtmlOutput "stack" opts

ghcPkgHaddockInterface :: {- [String] -> GhcPkgFixmeOptions -> -} String -> IO (Maybe String)
ghcPkgHaddockInterface {- allGhcOptions (GhcPkgFixmeOptions extraGHCPkgOpts) -} p =
    shortcut [ stackGhcPkgHaddockInterface
             , cabalPkgHaddockInterface
             , _ghcPkgHaddockInterface
             ]

  where

    _ghcPkgHaddockInterface :: IO (Maybe String)
    _ghcPkgHaddockInterface = do
        let opts = ["field", p, "haddock-interfaces"] ++ ["--global", "--user"] ++ optsForGhcPkg [] {- allGhcOptions ++ extraGHCPkgOpts -}
        putStrLn $ "ghc-pkg "++ show opts

        x <- executeFallibly' "ghc-pkg" opts

        return $ case x of
            Nothing         -> Nothing
            Just (hout, _)  -> Safe.lastMay $ words $ reverse . dropWhile (== '\n') . reverse $ hout

    -- | Call cabal sandbox hc-pkg to find the haddock Interfaces.
    cabalPkgHaddockInterface :: IO (Maybe String)
    cabalPkgHaddockInterface = do
        let opts = ["sandbox", "hc-pkg", "field", p, "haddock-interfaces"]
        putStrLn $ "cabal sandbox hc-pkg field " ++ p ++ " haddock-interfaces"

        x <- executeFallibly' "cabal" opts

        case x of
            Nothing         -> return Nothing
            Just (hout, _)  -> do let line = reverse . dropWhile (== '\n') . reverse $ hout
                                  print ("ZZZZZZZZZZZZZ", line)

                                  return $ if "haddock-interfaces" `isInfixOf` line
                                      then Safe.lastMay $ words line
                                      else Nothing

    -- | Call stack to find the haddock Interfaces.
    stackGhcPkgHaddockInterface :: IO (Maybe String)
    stackGhcPkgHaddockInterface = do
        let opts = ["exec", "ghc-pkg", "field", p, "haddock-interfaces"]
        putStrLn $ "stack exec ghc-pkg field " ++ p ++ " haddock-interfaces"

        x <- executeFallibly' "stack" opts

        case x of
            Nothing         -> return Nothing
            Just (hout, _)  -> do let line = reverse . dropWhile (== '\n') . reverse $ hout
                                  print ("UUUUUUUUUUUUU", line, opts)

                                  return $ if "haddock-interfaces" `isInfixOf` line
                                      then Safe.lastMay $ words line
                                      else Nothing

getVisibleExports
    :: forall m. (GhcMonad m,
                  MonadIO  m)
    => {- [String] -> GhcPkgFixmeOptions -> -}
       String
    -> m (Maybe (M.Map String [String]))
getVisibleExports {- allGhcOptions (GhcPkgFixmeOptions extraGHCPkgOpts) -} p = do
    haddockInterfaceFile <- liftIO $ ghcPkgHaddockInterface {- allGhcOptions (GhcPkgFixmeOptions extraGHCPkgOpts) -} p
    join <$> traverse getVisibleExports' haddockInterfaceFile

------------------------------------------------------------------------------------------------------------------------
-- Copied from http://hackage.haskell.org/package/haddock-api-2.16.1/docs/src/Haddock-InterfaceFile.html#nameCacheFromGhc
-- but for a general monad m instead of the specific monad Ghc.
------------------------------------------------------------------------------------------------------------------------
nameCacheFromGhc :: forall m. (GhcMonad m, MonadIO m) => Haddock.NameCacheAccessor m
nameCacheFromGhc = ( read_from_session , write_to_session )
  where
    read_from_session = do
       ref <- GhcMonad.withSession (return . hsc_NC)
       liftIO $ readIORef ref
    write_to_session nc' = do
       ref <- GhcMonad.withSession (return . hsc_NC)
       liftIO $ writeIORef ref nc'

getVisibleExports' :: forall m. (GhcMonad m, MonadIO m) => FilePath -> m (Maybe (M.Map String [String]))
getVisibleExports' ifile = do
    iface <- Haddock.readInterfaceFile nameCacheFromGhc ifile

    case iface of
        Left _          -> liftIO $ do putStrLn $ "Failed to read the Haddock interface file: " ++ ifile
                                       putStrLn "You probably installed packages without using the '--enable-documentation' flag."
                                       putStrLn ""
                                       putStrLn "Try something like:\n\n\tcabal install --enable-documentation p"
                                       error "No haddock interfaces file, giving up."
        Right iface'    -> do let m  = map (\ii -> (Haddock.instMod ii, Haddock.instVisibleExports ii)) $ Haddock.ifInstalledIfaces iface' :: [(Module, [Name])]
                                  m' = map (\(mname, names) -> (showSDoc tdflags $ ppr mname, map (showSDoc tdflags . ppr) names)) m       :: [(String, [String])]
                              return $ Just $ M.fromList m'



-- | Convert a module name string, e.g. @Data.List@ to @Data-List.html@.
moduleNameToHtmlFile :: String -> String
moduleNameToHtmlFile m =  map f m ++ ".html"
    where f :: Char -> Char
          f '.' = '-'
          f c   = c

{-
I don't want to use this any more. The refiner works so much better with
the local haddock interfaces file...

-- | Convert a file path to a Hackage HTML file to its equivalent on @https://hackage.haskell.org@.
toHackageUrl :: FilePath -> String -> String -> String
toHackageUrl filepath package modulename = "https://hackage.haskell.org/package/" ++ package ++ "/" ++ "docs/" ++ modulename''
    where filepath'    = map repl filepath
          modulename'  = head $ separateBy '.' $ head $ separateBy '-' modulename
          modulename'' = drop (fromJust $ substringP modulename' filepath') filepath'

          -- On Windows we get backslashes in the file path; convert
          -- to forward slashes for the URL.
          repl :: Char -> Char
          repl '\\' = '/'
          repl c    = c

          -- Adapted from http://www.haskell.org/pipermail/haskell-cafe/2010-June/078702.html
          substringP :: String -> String -> Maybe Int
          substringP _ []  = Nothing
          substringP sub str = if sub `isPrefixOf` str then Just 0 else (+1) <$> substringP sub (tail str)

-- | Convert our match to a URL, either @file://@ if the file exists, or to @hackage.org@ otherwise.
matchToUrl :: (Maybe String, Maybe String, Maybe String, Maybe String) -> IO String
matchToUrl (importedFrom, haddock, foundModule, base) = do
    when (isNothing importedFrom) $ error "importedFrom is Nothing :("
    when (isNothing haddock) $ error "haddock is Nothing :("
    when (isNothing foundModule) $ error "foundModule is Nothing :("
    when (isNothing base) $ error "base is Nothing :("

    let importedFrom' = fromJust importedFrom
        haddock'      = fromJust haddock
        foundModule'  = fromJust foundModule
        base'         = fromJust base

        f = haddock' </> base'

    e <- doesFileExist f

    if e then return $ "file://" ++ f
         else do putStrLn $ "f:  " ++ show f
                 putStrLn $ "foundModule2: " ++ show foundModule'
                 putStrLn $ "calling toHackageUrl with params: " ++ show (f, foundModule', importedFrom')
                 return $ toHackageUrl f foundModule' importedFrom'
-}

-- | Convert our match to a URL of the form @file://@ so that we can open it in a web browser.
matchToUrl :: (Maybe String, Maybe String, Maybe String, Maybe String) -> IO String
matchToUrl (importedFrom, haddock, foundModule, base) = do
    when (isNothing importedFrom)   $ error "importedFrom is Nothing :("
    when (isNothing haddock)        $ error "haddock is Nothing :("
    when (isNothing foundModule)    $ error "foundModule is Nothing :("
    when (isNothing base)           $ error "base is Nothing :("

    let -- importedFrom' = fromJust importedFrom
        haddock'      = fromJust haddock
        -- foundModule'  = fromJust foundModule
        base'         = fromJust base

        f = haddock' </> base'

    e <- doesFileExist f

    if e then return $ "file://" ++ f
         else do putStrLn "Please reinstall packages using the flag '--enable-documentation' for 'cabal install.\n"
                 error $ "Could not find " ++ f

filterMatchingQualifiedImport :: String -> [HaskellModule] -> [HaskellModule]
filterMatchingQualifiedImport symbol hmodules =
    case moduleOfQualifiedName symbol of Nothing    -> []
                                         asBit@(Just _) -> filter (\z -> asBit == modImportedAs z) hmodules

getModuleExports
    :: forall m. (GhcMonad m,
                  MonadIO  m)
    => {- GhcFixmeOptions
    -> GhcPkgFixmeOptions
    -> -} HaskellModule
    -> m (Maybe ([String], String))
getModuleExports {- (GhcFixmeOptions gopts) ghcpkgOpts -} m = do
    minfo     <- (findModule (mkModuleName $ modName m) Nothing >>= getModuleInfo)
                   `gcatch` (\(_  :: SourceError)   -> return Nothing)

    p <- liftIO $ ghcPkgFindModule {- gopts ghcpkgOpts -} (modName m)

    case (minfo, p) of
        (Nothing, _)            -> return Nothing
        (_, Nothing)            -> return Nothing
        (Just minfo', Just p')  -> return $ Just (map (showSDocForUser tdflags reallyAlwaysQualify . ppr) $ modInfoExports minfo', p')

-- type UnqualifiedName    = String    -- ^ e.g. "Just"
type FullyQualifiedName = String    -- ^ e.g. e.g. "base-4.8.2.0:Data.Foldable.length"
type StrModuleName      = String    -- ^ e.g. "Data.List"

data MySymbol = MySymbolSysQualified  String  -- ^ e.g. "base-4.8.2.0:Data.Foldable.length"
              | MySymbolUserQualified String  -- ^ e.g. "DL.length" with an import earlier like "import qualified Data.List as DL"
              deriving Show

data ModuleExports = ModuleExports
    { mName            :: StrModuleName            -- ^ e.g. "Data.List"
    , mPackageName     :: String                   -- ^ e.g. "snap-0.14.0.6"
    , mInfo            :: HaskellModule            -- ^ Our parse of the module import, with info like "hiding (map)".
    , qualifiedExports :: [FullyQualifiedName]     -- ^ e.g. [ "base-4.8.2.0:GHC.Base.++"
                                                        --        , "base-4.8.2.0:GHC.List.filter"
                                                        --        , "base-4.8.2.0:GHC.List.zip"
                                                        --        , ...
                                                        --        ]
    }
    deriving Show

pprModuleExports :: ModuleExports -> String
pprModuleExports me = mName me ++ "\n" ++ show (mInfo me) ++ "\n" ++ unwords (map show $ qualifiedExports me)

refineAs :: MySymbol -> [ModuleExports] -> [ModuleExports]

-- User qualified the symbol, so we can filter out anything that doesn't have a matching 'modImportedAs'.
refineAs (MySymbolUserQualified userQualSym) exports = filter f exports
  where
    f export = case modas of
                Nothing     -> False
                Just modas' -> modas' == userQualAs
       where modas = modImportedAs $ mInfo export :: Maybe String

             -- e.g. "DL"
             userQualAs = fromMaybe (error $ "Expected a qualified name like 'DL.length' but got: " ++ userQualSym)
                                    (moduleOfQualifiedName userQualSym)

-- User didn't qualify the symbol, so we have the full system qualified thing, so do nothing here.
refineAs (MySymbolSysQualified _) exports = exports

refineRemoveHiding :: [ModuleExports] -> [ModuleExports]
refineRemoveHiding exports = map (\e -> e { qualifiedExports = f e }) exports
  where
    f export = filter (`notElem` hiding') thisExports
       where hiding = modHiding $ mInfo export :: [String] -- Things that this module hides.
             hiding' = map (qualifyName thisExports) hiding  :: [String]    -- Qualified version of hiding.
             thisExports = qualifiedExports export         -- Things that this module exports.

    qualifyName :: [QualifiedName] -> FixmeSymbol -> QualifiedName
    qualifyName qualifiedNames name
        -- = case filter (postfixMatch name) qualifiedNames of
        = case nub (filter (name `f`) qualifiedNames) of
            [match]     -> match
            m           -> error $ "Could not qualify " ++ name ++ " from these exports: " ++ show qualifiedNames ++ "\n    matches: " ++ show m

        -- Time for some stringly typed rubbish. The previous test used
        -- postfixMatch but this failed on an import that had "hiding (lines, unlines)" since
        -- both lines and unlines matched. Prepending a dot doesn't work due to things like ".=" from
        -- Control.Lens. So we manually check that the suffix matches, that the next symbol is a dot,
        -- and then an alpha character, which hopefully is the end of a module name. Such a mess.
        where f n qn = if length qn - length n - 2 >= 0
                            then n `isSuffixOf` qn && isAlpha (qn !! (length qn - length n - 2)) && (qn !! (length qn - length n - 1)) == '.'
                            else error $ "Internal error: trying to check if \"" ++ n ++ "\" is a match for \"" ++ qn ++ "\""

refineExportsIt :: String -> [ModuleExports] -> [ModuleExports]
refineExportsIt symbol exports = map (\e -> e { qualifiedExports = f symbol e }) exports
  where
    -- f symbol export = filter (symbol ==) thisExports
    f sym export = filter (postfixMatch sym) thisExports
       where thisExports = qualifiedExports export         -- Things that this module exports.

refineLeadingDot :: MySymbol -> [ModuleExports] -> [ModuleExports]
refineLeadingDot (MySymbolUserQualified _)           exports = exports
refineLeadingDot (MySymbolSysQualified symb)         exports = map (\e -> e { qualifiedExports = f leadingDot e }) exports
  where
    leadingDot :: String
    leadingDot = '.' : last (separateBy '.' symb)

    -- f symbol export = filter (symbol ==) thisExports
    f symbol export = filter (symbol `isSuffixOf`) thisExports
       where thisExports = qualifiedExports export         -- Things that this module exports.

refineVisibleExports
    :: forall m. (GhcMonad m,
                  MonadIO  m)
    => {- [String] -> GhcPkgFixmeOptions -> -}
       [ModuleExports]
    -> m [ModuleExports]
refineVisibleExports {- allGhcOpts ghcPkgFixmeOptions -} exports = mapM f exports
  where
    f :: ModuleExports -> m ModuleExports
    f mexports = do
        let pname          = mPackageName     mexports -- e.g. "base-4.8.2.0"
            thisModuleName = mName            mexports -- e.g. "Prelude"
            qexports       = qualifiedExports mexports -- e.g. ["base-4.8.2.0:GHC.Base.Just", ...]
        visibleExportsMap <- getVisibleExports {- allGhcOpts ghcPkgFixmeOptions -} pname
        liftIO $ print visibleExportsMap

        let thisModVisibleExports = fromMaybe
                                        (error $ "Could not get visible exports of " ++ pname)
                                        (join $ traverse (M.lookup thisModuleName) visibleExportsMap)

        let qexports' = filter (hasPostfixMatch thisModVisibleExports) qexports

        liftIO $ print (qexports, qexports')

        return $ mexports { qualifiedExports = qexports' }

    -- hasPostfixMatch "base-4.8.2.0:GHC.Base.Just" ["Just", "True", ...] -> True
    hasPostfixMatch :: [String] -> String -> Bool
    hasPostfixMatch xs s = last (separateBy '.' s) `elem` xs

-- | The last thing with a single export must be the match? Iffy.
getLastMatch :: [ModuleExports] -> Maybe ModuleExports
getLastMatch exports = Safe.lastMay $ filter f exports
  where
    f me = length (qualifiedExports me) == 1

-- | Attempt to guess the Haddock url, either a local file path or url to @hackage.haskell.org@
-- for the symbol in the given file, module, at the specified line and column location.
--
-- Example:
--
-- >>> guessHaddockUrl "tests/data/data/Muddle.hs" "Muddle" "Maybe" 11 11
-- (lots of output)
-- SUCCESS: file:///home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/Data-Maybe.html

guessHaddockUrl
    :: forall m.
       (GhcMonad m,
        MonadIO  m)
    => ModSummary
    -> Cradle
    -> FilePath
    -> String
    -> FixmeSymbol
    -> Int
    -> Int
    -- -> GhcFixmeOptions
    -- -> GhcPkgFixmeOptions
    -> m (Either String String)
guessHaddockUrl modSum crdl _targetFile targetModule symbol lineNr colNr {- (GhcFixmeOptions ghcOpts0) ghcPkgFixmeOptions -} = do
    -- let targetFile = currentDir </> _targetFile
    let targetFile = _targetFile

    liftIO $ putStrLn $ "targetFile: " ++ targetFile
    liftIO $ putStrLn $ "targetModule: " ++ targetModule
    liftIO $ putStrLn $ "symbol: " ++ show symbol
    liftIO $ putStrLn $ "line nr: " ++ show lineNr
    liftIO $ putStrLn $ "col nr: " ++ show colNr

    -- liftIO $ putStrLn $ "ghcOpts0: " ++ show ghcOpts0
    -- liftIO $ putStrLn $ "GhcPkgFixmeOptions: " ++ show ghcPkgFixmeOptions

    let allGhcOpts      = undefined
        textualImports  = ms_textual_imps modSum

    let haskellModules0 = map toHaskellModule textualImports
        haskellModuleNames0 = map modName haskellModules0
    liftIO $ putStrLn $ "haskellModuleNames0: " ++ show haskellModuleNames0
    liftIO $ putStrLn $ "haskellModuleNames0 (full detail): " ++ show haskellModules0

    -- If symbol is something like DM.lookup, then restrict haskellModuleNames to the
    -- one that has modImportedAs == Just "DM".
    let filterThings = filterMatchingQualifiedImport symbol haskellModules0
    -- let haskellModules = if null filterThings then haskellModules0 else filterThings
    let haskellModuleNames = if null filterThings then map modName haskellModules0 else map modName filterThings

    qnames <- filter (not . (' ' `elem`)) <$> qualifiedName targetModule lineNr colNr haskellModuleNames
    liftIO $ putStrLn $ "qualified names: " ++ show qnames

    qnames_with_qualified_printing <- filter (not . (' ' `elem`)) <$> qualifiedName' targetModule lineNr colNr symbol haskellModuleNames :: m [String]
    liftIO $ putStrLn $ "qualified names with qualified printing: " ++ show qnames_with_qualified_printing

    let parsedPackagesAndQualNames :: [Either TP.ParseError (String, String)]
        parsedPackagesAndQualNames = map (TP.parse parsePackageAndQualName "") qnames_with_qualified_printing

    liftIO $ putStrLn $ "qqqqqq1: " ++ show parsedPackagesAndQualNames

    let symbolToUse :: String
        symbolToUse = case (qnames_with_qualified_printing, qnames) of
                        (qq:_, _)     -> qq   -- We got a qualified name, with qualified printing. Qualified!
                        ([], qn:_)    -> qn   -- No qualified names (oh dear) so fall back to qnames list.
                        ([], [])        -> error "Lists 'qnames' and 'qnames_with_qualified_printing' are both empty."

    liftIO $ print ("symbolToUse", symbolToUse)

    -- Possible extra modules...
    let extraModules :: [HaskellModule]
        extraModules = case Safe.headMay parsedPackagesAndQualNames of
                        Just (Right (_, x)) -> case moduleOfQualifiedName x of Just x' -> [ HaskellModule { modName         = x'
                                                                                                          , modQualifier    = Nothing
                                                                                                          , modIsImplicit   = False
                                                                                                          , modHiding       = []
                                                                                                          , modImportedAs   = Nothing
                                                                                                          , modSpecifically = []
                                                                                                          }
                                                                                          ]
                                                                               Nothing -> []
                        _                   -> []

    liftIO $ print extraModules

    -- Try to use the qnames_with_qualified_printing case, which has something like "base-4.8.2.0:GHC.Base.map",
    -- which will be more accurate to filter on.

    exports <- mapM (getModuleExports {- (GhcFixmeOptions ghcOpts0) ghcPkgFixmeOptions -} ) (haskellModules0 ++ extraModules)

    -- Sometimes the modules in extraModules might be hidden or weird ones like GHC.Base that we can't
    -- load, so filter out the successfully loaded ones.
    let successes :: [(HaskellModule, Maybe ([String], String))]
        successes = filter (isJust . snd) (zip (haskellModules0 ++ extraModules) exports)

        bubble :: (HaskellModule, Maybe ([FullyQualifiedName], String)) -> Maybe (HaskellModule, ([FullyQualifiedName], String))
        bubble (h, Just x)  = Just (h, x)
        bubble (_, Nothing) = Nothing

        successes' :: [(HaskellModule, ([String], String))]
        successes' = mapMaybe bubble successes

        upToNow = map (\(m, (e, p)) -> ModuleExports
                                            { mName             = modName m
                                            , mPackageName      = p
                                            , mInfo             = m
                                            , qualifiedExports  = e
                                            }) successes'

    liftIO $ forM_ upToNow $ \x -> putStrLn $ pprModuleExports x

    -- Get all "as" imports.
    let asImports :: [String]
        asImports = mapMaybe (modImportedAs . mInfo) upToNow

    -- Can a user do "import xxx as Foo.Bar"??? Check this.

    let mySymbol = case moduleOfQualifiedName symbol of
                    Nothing     -> MySymbolSysQualified symbolToUse
                    Just x      -> if x `elem` asImports
                                        then MySymbolUserQualified symbol
                                        else MySymbolSysQualified symbolToUse

    liftIO $ print mySymbol

    let upToNow0 = refineAs mySymbol upToNow
    liftIO $ putStrLn "upToNow0"
    liftIO $ forM_ upToNow0 $ \x -> putStrLn $ pprModuleExports x

    let upToNow1 = refineRemoveHiding upToNow0
    liftIO $ putStrLn "upToNow1"
    liftIO $ forM_ upToNow1 $ \x -> putStrLn $ pprModuleExports x

    let upToNow2 = refineExportsIt symbolToUse upToNow1
    liftIO $ putStrLn "upToNow2"
    liftIO $ forM_ upToNow2 $ \x -> putStrLn $ pprModuleExports x

    let upToNow3 = refineLeadingDot mySymbol upToNow2
    liftIO $ putStrLn "upToNow3"
    liftIO $ forM_ upToNow3 $ \x -> putStrLn $ pprModuleExports x

    upToNow4 <- refineVisibleExports {- allGhcOpts ghcPkgFixmeOptions -} upToNow3
    liftIO $ putStrLn "upToNow4"
    liftIO $ forM_ upToNow4 $ \x -> putStrLn $ pprModuleExports x

    let lastMatch3 = getLastMatch upToNow3
        lastMatch4 = getLastMatch upToNow4
        lastMatch  = Safe.headMay $ catMaybes [lastMatch4, lastMatch3]

    liftIO $ print $ "last match: " ++ show lastMatch

    -- "last match: Just (ModuleExports {mName = \"Control.Monad\", mInfo = HaskellModule {modName = \"Control.Monad\", modQualifier = Nothing, modIsImplicit = False, modHiding = [], modImportedAs = Nothing, modSpecifically = [\"forM_\",\"liftM\",\"filterM\",\"when\",\"unless\"]}, qualifiedExports = [\"base-4.8.2.0:GHC.Base.when\"]})"

    let matchedModule :: String
        matchedModule = case mName <$> lastMatch of
                            Just modn   -> modn
                            _           -> error $ "No nice match in lastMatch for module: " ++ show lastMatch

    let matchedPackageName :: String
        matchedPackageName = case mPackageName <$> lastMatch of
                                Just p -> p
                                _      -> error $ "No nice match in lastMatch for package name: " ++ show lastMatch

    haddock <- liftIO $ (maybe (return Nothing) (ghcPkgHaddockUrl {- allGhcOpts ghcPkgFixmeOptions -} ) . Just) matchedPackageName

    liftIO $ putStrLn $ "at the end now: " ++ show (matchedModule, moduleNameToHtmlFile matchedModule, matchedPackageName, haddock)

    url <- liftIO $ matchToUrl (Just matchedModule, haddock, Just matchedModule, Just $ moduleNameToHtmlFile matchedModule)

    return $ Right url

-- | Top level function; use this one from src/Main.hs.
haddockUrl :: forall m. (GhcMonad m, MonadIO m) => ModSummary -> Cradle -> Options -> FilePath -> String -> String -> Int -> Int -> m String
haddockUrl modSum crdl opt file modstr symbol lineNr colNr = do

    let ghcopts    = undefined -- GhcOptions    $ ghcOpts    opt
    let ghcpkgopts = undefined -- GhcPkgOptions $ ghcPkgOpts opt

    res <- guessHaddockUrl modSum crdl file modstr symbol lineNr colNr {- ghcopts ghcpkgopts -}
    liftIO $ print ("res", show res)

    case res of Right x  -> return $ "SUCCESS: " ++ x ++ "\n"
                Left err -> return $ "FAIL: " ++ show err ++ "\n"

-- | Look up Haddock docs for a symbol.
haddock :: IOish m
      => FilePath     -- ^ A target file.
      -> Int          -- ^ Line number.
      -> Int          -- ^ Column number.
      -> GhcModT m String
haddock file lineNo colNo =
  ghandle handler $
    runGmlT' [Left file] deferErrors $
      withInteractiveContext $ do
        crdl         <- cradle
        modSum       <- fileModSummaryWithMapping (cradleCurrentDir crdl </> file)
        -- srcSpanTypes <- getSrcSpanType modSum lineNo colNo
        -- dflag        <- G.getSessionDynFlags
        -- st           <- getStyle
        -- convert' $ map (toTup dflag st) $ sortBy (cmp `on` fst) srcSpanTypes
        let opt    = undefined
            modstr = undefined
            symbol = undefined
        blargh <- haddockUrl modSum crdl opt file modstr symbol lineNo colNo
        return blargh
 where
   handler (SomeException ex) = do
     gmLog GmException "haddock" $ showDoc ex
     return []
