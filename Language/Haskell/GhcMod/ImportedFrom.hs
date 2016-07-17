-- Copyright (C) 2013-2016 Carlo Hamalainen <carlo ÄT carlo-hamalainen DOT net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP, FlexibleContexts, Rank2Types, ScopedTypeVariables #-}

module Language.Haskell.GhcMod.ImportedFrom (importedFrom) where

import Control.Exception
import Control.Monad
import Data.Char (isAlpha)
import Data.Functor.Identity
import Data.IORef
import Data.List
import Data.List.Split
import Data.Maybe
import Exception (ghandle)
import FastString
import GHC
import HscTypes
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.FileMapping
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Output
import Language.Haskell.GhcMod.SrcUtils (listifySpans)
import Outputable
import System.Directory
import System.FilePath

import qualified Data.Map as M
import qualified Documentation.Haddock as Haddock
import qualified GhcMonad
import qualified Safe
import qualified SrcLoc
import qualified Text.Parsec as TP

type QualifiedName = String -- ^ A qualified name, e.g. @Foo.bar@.

data NiceImportDecl
    -- | Information about an import of a Haskell module. Convenience type
    -- for the bits of a 'GHC.ImportDecl' that we need.
    = NiceImportDecl
        { modName           :: String
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

-- | Convenience function for converting an 'GHC.ImportDecl' to a 'NiceImportDecl'.
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
-- >>> map toImportDecl <$> getTextualImports "tests/data/data/Hiding.hs" "Hiding" >>= print
-- [ NiceImportDecl { modName = "Prelude"
--                  , modQualifier = Nothing
--                  , modIsImplicit = True
--                  , modHiding = []
--                  , modImportedAs = Nothing
--                  , modSpecifically = []
--                  }
-- , NiceImportDecl {modName = "Safe"
--                  , modQualifier = Nothing
--                  , modIsImplicit = False
--                  , modHiding = []
--                  , modImportedAs = Nothing
--                  , modSpecifically = []
--                  }
-- , NiceImportDecl { modName = "System.Environment"
--                  , modQualifier = Nothing
--                  , modIsImplicit = False
--                  , modHiding = []
--                  , modImportedAs = Nothing
--                  , modSpecifically = ["getArgs"]
--                  }
-- , NiceImportDecl { modName = "Data.List"
--                  , modQualifier = Nothing
--                  , modIsImplicit = False
--                  , modHiding = ["map"]
--                  , modImportedAs = Nothing
--                  , modSpecifically = []
--                  }
-- ]
toImportDecl :: GHC.DynFlags -> SrcLoc.Located (GHC.ImportDecl GHC.RdrName) -> NiceImportDecl
toImportDecl dflags idecl = NiceImportDecl
                            { modName           = name
                            , modQualifier      = qualifier
                            , modIsImplicit     = isImplicit
                            , modHiding         = hiding
                            , modImportedAs     = importedAs
                            , modSpecifically   = specifically
                            }
  where
    idecl'       = SrcLoc.unLoc idecl
    name         = showSDoc dflags (ppr $ GHC.ideclName idecl')
    isImplicit   = GHC.ideclImplicit idecl'
    qualifier    = unpackFS <$> GHC.ideclPkgQual idecl'
    hiding       = (catMaybes . parseHiding . ghcIdeclHiding) idecl'
    importedAs   = (showSDoc dflags . ppr) <$> ideclAs idecl'
    specifically = (parseSpecifically . ghcIdeclHiding) idecl'

    grabNames :: GHC.Located [GHC.LIE GHC.RdrName] -> [String]
    grabNames loc = map (showSDoc dflags . ppr) names
      where names :: [RdrName]
            names = map (ieName . SrcLoc.unLoc) $ SrcLoc.unLoc loc
            -- FIXME We are throwing away location info by using unLoc each time?
            -- Trace these things to see what we are losing.

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
    parseHiding (Just (True, h))  = map Just $ grabNames h

    parseSpecifically :: Maybe (Bool, Located [LIE RdrName]) -> [String]
    parseSpecifically (Just (False, h)) = grabNames h
    parseSpecifically _                 = []

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

postfixMatch :: String -> QualifiedName -> Bool
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
-- Returns a fully qualified name thatincludes the package, hash, and name, e.g.
--
-- "containers-0.5.6.2@conta_2C3ZI8RgPO2LBMidXKTvIU:Data.Map.Base.fromList".
qualifiedName
    :: forall m. (GhcMonad m, MonadIO m, GmOut m, GmLog m)
    => String -> Int -> Int -> String -> [String] -> m [String]
qualifiedName targetModuleName lineNr colNr symbol importList = do
        dflags <- GHC.getSessionDynFlags

        setContext (map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList))
           `gcatch` (\(s  :: SourceError)    -> do gmLog GmDebug "qualifiedName" $ strDoc $ "qualifiedName: setContext failed with a SourceError, trying to continue anyway..." ++ show s
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(g  :: GhcApiError)    -> do gmLog GmDebug "qualifiedName" $ strDoc $ "qualifiedName: setContext failed with a GhcApiError, trying to continue anyway..." ++ show g
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(se :: SomeException)  -> do gmLog GmDebug "qualifiedName" $ strDoc $ "qualifiedName: setContext failed with a SomeException, trying to continue anyway..." ++ show se
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)

        modSummary <- getModSummary $ mkModuleName targetModuleName :: m ModSummary
        p <- parseModule modSummary                                 :: m ParsedModule
        t <- typecheckModule p                                      :: m TypecheckedModule

        let TypecheckedModule{tm_typechecked_source = tcs} = t
            bs = listifySpans tcs (lineNr, colNr) :: [LHsBind Id]
            es = listifySpans tcs (lineNr, colNr) :: [LHsExpr Id]
            ps = listifySpans tcs (lineNr, colNr) :: [LPat Id]

        let bs' = map (showSDocForUser dflags ghcQualify . ppr) bs
            es' = map (showSDocForUser dflags ghcQualify . ppr) es
            ps' = map (showSDocForUser dflags ghcQualify . ppr) ps

        return $ filter (postfixMatch symbol) $ concatMap words $ bs' ++ es' ++ ps'



ghcPkgFindModule
    :: forall m. (GhcMonad m, MonadIO m, GmOut m, GmLog m)
    => String
    -> m (Maybe String)
ghcPkgFindModule mod = do
    rp <- gmReadProcess

    shortcut [ stackGhcPkgFindModule rp mod
             , hcPkgFindModule       rp mod
             , _ghcPkgFindModule     rp mod
             ]
  where
    shortcut :: [m (Maybe a)] -> m (Maybe a)
    shortcut []     = return Nothing
    shortcut (a:as) = do
        a' <- a

        case a' of
            a''@(Just _)    -> return a''
            Nothing         -> shortcut as

    optsForGhcPkg :: [String] -> [String]
    optsForGhcPkg [] = []
    optsForGhcPkg ("-no-user-package-db":rest)   = "--no-user-package-db"          : optsForGhcPkg rest
    optsForGhcPkg ("-package-db":pd:rest)        = ("--package-db" ++ "=" ++ pd)   : optsForGhcPkg rest
    optsForGhcPkg ("-package-conf":pc:rest)      = ("--package-conf" ++ "=" ++ pc) : optsForGhcPkg rest
    optsForGhcPkg ("-no-user-package-conf":rest) = "--no-user-package-conf"        : optsForGhcPkg rest
    optsForGhcPkg (_:rest) = optsForGhcPkg rest

    runCmd rp cmd opts = liftIO ((Just <$> (rp cmd opts "")) `catch` (\(_::IOError) -> return Nothing))

    -- | Call @ghc-pkg find-module@ to determine that package that provides a module, e.g. @Prelude@ is defined
    -- in @base-4.6.0.1@.
    -- _ghcPkgFindModule :: String -> IO (Maybe String)
    _ghcPkgFindModule rp m = do
        let opts = ["find-module", m, "--simple-output"] ++ ["--global", "--user"] ++ optsForGhcPkg []
        gmLog GmDebug "_ghcPkgFindModule" $ strDoc $ "ghc-pkg " ++ show opts

        x <- runCmd rp "ghc-pkg" opts

        -- gmLog GmDebug "" $ strDoc $ "_ghcPkgFindModule stdout: " ++ show output
        -- gmLog GmDebug "" $ strDoc $ "_ghcPkgFindModule stderr: " ++ show err
        return $ case x of
                    Just x' -> join $ (Safe.lastMay . words) <$> (Safe.lastMay . lines) x'
                    Nothing -> Nothing

    -- | Call @cabal sandbox hc-pkg@ to find the package the provides a module.
    -- hcPkgFindModule :: String -> IO (Maybe String)
    hcPkgFindModule rp m = do
        let opts = ["sandbox", "hc-pkg", "find-module", m, "--", "--simple-output"]

        x <- runCmd rp "cabal" opts

        -- gmLog GmDebug "" $ strDoc $ "hcPkgFindModule stdout: " ++ show output
        -- gmLog GmDebug "" $ strDoc $ "hcPkgFindModule stderr: " ++ show err
        return $ case x of
                    Just x' -> join $ (Safe.lastMay . words) <$> (Safe.lastMay . lines) x'
                    Nothing -> Nothing

    -- | Call @stack exec ghc-pkg@ to find the package the provides a module.
    -- stackGhcPkgFindModule :: String -> IO (Maybe String)
    stackGhcPkgFindModule rp m = do
        let opts = ["exec", "ghc-pkg", "find-module", m, "--", "--simple-output"]

        x <- runCmd rp "stack" opts

        -- gmLog GmDebug "" $ strDoc $ "stackGhcPkgFindModule stdout: " ++ show output
        -- gmLog GmDebug "" $ strDoc $ "stackGhcPkgFindModule stderr: " ++ show err
        return $ case x of
                    Just x' -> join $ (Safe.lastMay . words) <$> (Safe.lastMay . lines) x'
                    Nothing -> Nothing

ghcPkgHaddockUrl
    :: forall m. (GmLog m, GmOut m, MonadIO m)
    => FilePath
    -> (FilePath -> [String] -> String -> IO String)
    -> [GhcPkgDb]
    -> String
    -> m (Maybe String)
ghcPkgHaddockUrl ghcPkg readProc pkgDbStack p = do
    gmLog GmDebug "ghcPkgHaddockUrl" $ strDoc p

    let p' = case splitOn "@" p of
                [p0, _] -> p0
                _       -> p

    hout <- liftIO $ readProc ghcPkg (toDocDirOpts p' pkgDbStack) ""
    return $ Safe.lastMay $ words $ reverse . dropWhile (== '\n') . reverse $ hout
  where
    -- This fails unless we have --global and --user, unlike
    -- pkgDoc elsewhere in ghc-mod.
    toDocDirOpts pkg dbs = ["field", pkg, "haddock-html", "--global", "--user"] ++ ghcPkgDbStackOpts dbs

ghcPkgHaddockInterface
    :: forall (m :: * -> *).  MonadIO m
    => FilePath
    -> (FilePath -> [String] -> String -> IO String)
    -> [GhcPkgDb]
    -> String
    -> m (Maybe String)
ghcPkgHaddockInterface ghcPkg readProc pkgDbStack p = do
    hout <- liftIO $ readProc ghcPkg (toHaskellInterfaces p pkgDbStack) ""
    return $ Safe.lastMay $ words $ reverse . dropWhile (== '\n') . reverse $ hout
  where
    toHaskellInterfaces pkg dbs = ["field", pkg, "haddock-interfaces", "--global", "--user"] ++ ghcPkgDbStackOpts dbs

getVisibleExports
    :: forall m. (GhcMonad m, MonadIO m, GmOut m, GmLog m)
    => (String -> m (Maybe String))
    -> String
    -> m (Maybe (M.Map String [String]))
getVisibleExports getHaddockInterfaces p = do
    gmLog GmDebug "getVisibleExports" $ strDoc p

    let p' = case splitOn "@" p of
                [p0, _] -> p0
                _       -> p

    haddockInterfaceFile <- getHaddockInterfaces p'

    case haddockInterfaceFile of
        Just hi -> getVisibleExports' hi
        Nothing -> return Nothing

  where

    getVisibleExports' :: forall m. (GhcMonad m, MonadIO m, GmOut m, GmLog m)
                       => FilePath
                       -> m (Maybe (M.Map String [String]))
    getVisibleExports' ifile = do
        iface <- Haddock.readInterfaceFile nameCacheFromGhc ifile

        dflags <- GHC.getSessionDynFlags

        case iface of
            Left _          -> do gmErrStrLn $ "Failed to read the Haddock interface file: " ++ ifile
                                            ++ "You probably installed packages without using the '--enable-documentation' flag."
                                            ++ ""
                                            ++ "Try something like:\n\n\tcabal install --enable-documentation p"
                                  error "No haddock interfaces file, giving up."
            Right iface'    -> do let m  = map (\ii -> (Haddock.instMod ii, Haddock.instVisibleExports ii)) $ Haddock.ifInstalledIfaces iface' :: [(Module, [Name])]
                                      m' = map (\(mname, names) -> (showSDoc dflags $ ppr mname, map (showSDoc dflags . ppr) names)) m       :: [(String, [String])]
                                  return $ Just $ M.fromList m'

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

getModuleExports
    :: forall m. (GhcMonad m, MonadIO m, GmOut m, GmLog m)
    => FilePath
    -> (FilePath -> [String] -> String -> IO String)
    -> [GhcPkgDb]
    -> NiceImportDecl
    -> m (Maybe ([String], String))
getModuleExports ghcPkg readProc pkgDbStack m = do
    minfo     <- (findModule (mkModuleName $ modName m) Nothing >>= getModuleInfo)
                   `gcatch` (\(_  :: SourceError)   -> return Nothing)

    p <- ghcPkgFindModule $ modName m

    dflags <- GHC.getSessionDynFlags

    case (minfo, p) of
        (Nothing, _)            -> return Nothing
        (_, Nothing)            -> return Nothing
        (Just minfo', Just p')  -> return $ Just (map (showSDocForUser dflags ghcQualify . ppr) $ modInfoExports minfo', p')

type FullyQualifiedName = String    -- ^ e.g. e.g. "base-4.8.2.0:Data.Foldable.length"
type StrModuleName      = String    -- ^ e.g. "Data.List"

data MySymbol = MySymbolSysQualified  String  -- ^ e.g. "base-4.8.2.0:Data.Foldable.length"
              | MySymbolUserQualified String  -- ^ e.g. "DL.length" with an import earlier like "import qualified Data.List as DL"
              deriving Show

data ModuleExports = ModuleExports
    { mName            :: StrModuleName            -- ^ e.g. "Data.List"
    , mPackageName     :: String                   -- ^ e.g. "snap-0.14.0.6"
    , mInfo            :: NiceImportDecl           -- ^ Our parse of the module import, with info like "hiding (map)".
    , qualifiedExports :: [FullyQualifiedName]     -- ^ e.g. [ "base-4.8.2.0:GHC.Base.++"
                                                   --        , "base-4.8.2.0:GHC.List.filter"
                                                   --        , "base-4.8.2.0:GHC.List.zip"
                                                   --        , ...
                                                   --        ]
    }
    deriving Show

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

    qualifyName :: [QualifiedName] -> String -> QualifiedName
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
    :: forall m. (GhcMonad m, MonadIO m, GmOut m, GmLog m)
    => (String -> m (Maybe String))
    -> [ModuleExports]
    -> m [ModuleExports]
refineVisibleExports getHaddockInterfaces exports = mapM f exports
  where
    f :: ModuleExports -> m ModuleExports
    f mexports = do
        let pname          = mPackageName     mexports -- e.g. "base-4.8.2.0"
            thisModuleName = mName            mexports -- e.g. "Prelude"
            qexports       = qualifiedExports mexports -- e.g. ["base-4.8.2.0:GHC.Base.Just", ...]
        mVisibleExportsMap <- getVisibleExports getHaddockInterfaces pname

        --let thisModVisibleExports = fromMaybe
        --                                (error $ "Could not get visible exports of <" ++ thisModuleName ++ "> in " ++ pname)
        --                                (case visibleExportsMap of
        --                                    Just ve -> M.lookup thisModuleName ve
        --                                    Nothing -> Nothing)
        let visibleExportsMap = fromMaybe (error $ "visible exports map is Nothing") mVisibleExportsMap
        gmLog GmDebug "visibleExportsMap" $ strDoc $ show visibleExportsMap

        let thisModVisibleExports0 = M.lookup thisModuleName visibleExportsMap

        -- On earlier versions of GHC, our qexports list will not be fully qualified, so it will
        -- look like ["base:GHC.Base.Just", ...] instead of ["base-4.8.2.0:GHC.Base.Just", ...].
        -- So if thisModVisibleExports0 is Nothing, fall back to searching on a shorter pname.
        let thisModVisibleExports = case thisModVisibleExports0 of
                                        Just ve -> ve
                                        Nothing -> let pname' = ((head $ separateBy '-' pname) ++ ":" ++ thisModuleName) in
                                                        fromMaybe
                                                            (error $ "Failed to find visible exports map in fall-back case: " ++ show (pname', thisModuleName))
                                                            (M.lookup pname' visibleExportsMap)

        let qexports' = filter (hasPostfixMatch thisModVisibleExports) qexports

        gmLog GmDebug "visibleExportsMap" $ strDoc $ show (qexports, qexports')

        return $ mexports { qualifiedExports = qexports' }

    -- hasPostfixMatch "base-4.8.2.0:GHC.Base.Just" ["Just", "True", ...] -> True
    hasPostfixMatch :: [String] -> String -> Bool
    hasPostfixMatch xs s = last (separateBy '.' s) `elem` xs

-- | The last thing with a single export must be the match? Iffy.
getLastMatch :: [ModuleExports] -> Maybe ModuleExports
getLastMatch exports = Safe.lastMay $ filter f exports
  where
    f me = length (qualifiedExports me) == 1

-- | Try to look up the Haddock URL for a symbol.
guessHaddockUrl
    :: forall m.
       (GhcMonad m, MonadIO m, GmOut m, GmLog m)
    => ModSummary
    -> FilePath
    -> String
    -> String
    -> Int
    -> Int
    -> FilePath
    -> (FilePath -> [String] -> String -> IO String)
    -> [GhcPkgDb]
    -> m (Either String String)
guessHaddockUrl modSum targetFile targetModule symbol lineNr colNr ghcPkg readProc pkgDbStack = do
    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "targetFile: "   ++ targetFile
    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "targetModule: " ++ targetModule
    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "symbol: "       ++ show symbol
    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "line nr: "      ++ show lineNr
    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "col nr: "       ++ show colNr

    dflags <- GHC.getSessionDynFlags

    let textualImports  = ms_textual_imps modSum
        importDecls0 = map (toImportDecl dflags) textualImports

    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "haskellModuleNames0: " ++ show importDecls0

    -- If symbol is something like DM.lookup, then restrict importDecls0 to the
    -- one that has modImportedAs == Just "DM".
    let importDecls1 = filterMatchingQualifiedImport symbol importDecls0

    -- If that filter left us with nothing, revert back to the original list.
    let importDecls2 = if null importDecls1
                                then importDecls0
                                else importDecls1

    qnames <- filter (not . (' ' `elem`)) <$> qualifiedName targetModule lineNr colNr symbol (map modName importDecls2) :: m [String]
    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "qnames: " ++ show qnames

    let symbolToUse :: String
        symbolToUse = case qnames of
                        (qq:_) -> qq -- We got a qualified name, with qualified printing. Qualified!
                        []     -> error "qnames is empty."

    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "symbolToUse: " ++ symbolToUse

    -- Sometimes we have to load an extra module (using setContext) otherwise
    -- we can't look up the global reader environment without causing a GHC panic.
    -- For example 'Int' comes from GHC.Types, which is picked up here via the
    -- full qualified name.
    let parsedPackagesAndQualNames :: [Either TP.ParseError (String, String)]
        parsedPackagesAndQualNames = map (TP.parse parsePackageAndQualName "") qnames

        extraImportDecls :: [NiceImportDecl]
        extraImportDecls = case Safe.headMay parsedPackagesAndQualNames of
                        Just (Right (_, x)) -> case moduleOfQualifiedName x of Just x' -> [ NiceImportDecl
                                                                                                { modName         = x'
                                                                                                , modQualifier    = Nothing
                                                                                                , modIsImplicit   = False
                                                                                                , modHiding       = []
                                                                                                , modImportedAs   = Nothing
                                                                                                , modSpecifically = []
                                                                                                }
                                                                                          ]
                                                                               Nothing -> []
                        _                   -> []

        importDecls3 = importDecls2 ++ extraImportDecls

    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "parsedPackagesAndQualNames: " ++ show parsedPackagesAndQualNames
    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "extraImportDecls: " ++ show extraImportDecls

    exports0 <- mapM (getModuleExports ghcPkg readProc pkgDbStack) importDecls3 :: m [Maybe ([String], String)]

    -- Sometimes the modules in extraImportDecls might be hidden or weird ones like GHC.Base that we can't
    -- load, so filter out the successfully loaded ones.
    let successes :: [(NiceImportDecl, Maybe ([String], String))]
        successes = filter (isJust . snd) (zip importDecls3 exports0)

        toMaybe :: (NiceImportDecl, Maybe ([FullyQualifiedName], String)) -> Maybe (NiceImportDecl, ([FullyQualifiedName], String))
        toMaybe (h, Just x)  = Just (h, x)
        toMaybe (_, Nothing) = Nothing

        successes' :: [(NiceImportDecl, ([String], String))]
        successes' = mapMaybe toMaybe successes

        stage0 = map (\(m, (e, p)) -> ModuleExports
                                            { mName             = modName m
                                            , mPackageName      = p
                                            , mInfo             = m
                                            , qualifiedExports  = e
                                            }) successes'

    -- Get all "as" imports.
    let asImports :: [String]
        asImports = mapMaybe (modImportedAs . mInfo) stage0

        mySymbol = case moduleOfQualifiedName symbol of
                    Nothing     -> MySymbolSysQualified symbolToUse
                    Just x      -> if x `elem` asImports
                                        then MySymbolUserQualified symbol
                                        else MySymbolSysQualified symbolToUse

    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "mySymbol: " ++ show mySymbol

    let pprModuleExports :: ModuleExports -> String
        pprModuleExports me = "(" ++ mName me ++ ", " ++ show (mInfo me) ++ ", " ++ unwords (map show $ qualifiedExports me) ++ ")"

        showDebugStage stageNr stage = forM_ stage $ \x -> gmLog GmDebug "guessHaddockUrl" $ strDoc $ stageNr ++ " " ++ pprModuleExports x

    showDebugStage "stage0" stage0

    let stage1 = refineAs mySymbol stage0
    showDebugStage "stage1" stage1

    let stage2 = refineRemoveHiding stage1
    showDebugStage "stage2" stage2

    let stage3 = refineExportsIt symbolToUse stage2
    showDebugStage "stage3" stage3

    let stage4 = refineLeadingDot mySymbol stage3
    showDebugStage "stage4" stage4

    stage5 <- refineVisibleExports (ghcPkgHaddockInterface ghcPkg readProc pkgDbStack) stage4
    showDebugStage "stage5" stage5

    let lastMatch  = Safe.headMay $ catMaybes [getLastMatch stage5, getLastMatch stage4]

    gmLog GmDebug "guessHaddockUrl" $ strDoc $ show $ "lastMatch: " ++ show lastMatch

    let lastMatchModule :: String
        lastMatchModule = case mName <$> lastMatch of
                            Just modn   -> modn
                            _           -> error $ "No nice match in lastMatch for module: " ++ show lastMatch

        lastMatchPackageName :: String
        lastMatchPackageName = case mPackageName <$> lastMatch of
                                Just p -> p
                                _      -> error $ "No nice match in lastMatch for package name: " ++ show lastMatch

    let getHaddockUrl = ghcPkgHaddockUrl ghcPkg readProc pkgDbStack :: String -> m (Maybe String)

    haddock <- (maybe (return Nothing) getHaddockUrl . Just) lastMatchPackageName

    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "lastMatchModule:      " ++ lastMatchModule
    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "lastMatchPackageName: " ++ lastMatchPackageName
    gmLog GmDebug "guessHaddockUrl" $ strDoc $ "haddock:              " ++ show haddock

    haddock <- return $ fromMaybe (error "haddock is Nothing :(") haddock

    let f = haddock </> (moduleNameToHtmlFile lastMatchModule)

    e <- liftIO $ doesFileExist f

    if e then return $ Right $ "file://" ++ f
         else do gmErrStrLn "Please reinstall packages using the flag '--enable-documentation' for 'cabal install.\n"
                 return $ Left $ "Could not find " ++ f

  where
    -- Convert a module name string, e.g. @Data.List@ to @Data-List.html@.
    moduleNameToHtmlFile :: String -> String
    moduleNameToHtmlFile m = map f m ++ ".html"
      where
        f :: Char -> Char
        f '.' = '-'
        f c   = c

    filterMatchingQualifiedImport :: String -> [NiceImportDecl] -> [NiceImportDecl]
    filterMatchingQualifiedImport symbol hmodules
        = case moduleOfQualifiedName symbol of
            Nothing        -> []
            asBit@(Just _) -> filter (\z -> asBit == modImportedAs z) hmodules

-- | Look up Haddock docs for a symbol.
importedFrom
    :: forall m. IOish m
    => FilePath     -- ^ A target file.
    -> Int          -- ^ Line number.
    -> Int          -- ^ Column number.
    -> Expression   -- ^ Expression (symbol)
    -> GhcModT m String
importedFrom file lineNr colNr (Expression symbol) = do
    ghcPkg     <- getGhcPkgProgram
    readProc   <- gmReadProcess
    pkgDbStack <- getPackageDbStack

    ghandle handler $
      runGmlT' [Left file] deferErrors $
        withInteractiveContext $ do
          crdl   <- cradle
          modSum <- fileModSummaryWithMapping (cradleCurrentDir crdl </> file)
          let modstr = moduleNameString $ ms_mod_name modSum :: String

          res <- guessHaddockUrl modSum file modstr symbol lineNr colNr ghcPkg readProc pkgDbStack

          case res of Right x  -> return $ "SUCCESS: " ++ x ++ "\n"
                      Left err -> return $ "FAIL: " ++ show err ++ "\n"
  where
    handler (SomeException ex) = do
      gmLog GmException "imported-from" $ showDoc ex
      return []
