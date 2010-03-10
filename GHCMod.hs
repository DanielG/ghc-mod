module Main where

import Control.Exception hiding (try)
import Data.Char
import Data.List
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser hiding (parse)
import Language.Haskell.Exts.Syntax
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Process
import Text.ParserCombinators.Parsec

----------------------------------------------------------------

usage :: String
usage =    "ghc-mod version 0.1.0\n"
        ++ "Usage:\n"
        ++ "\t ghc-mod list\n"
        ++ "\t ghc-mod browse <module>\n"
        ++ "\t ghc-mod help\n"

----------------------------------------------------------------

data Options   = Options { optToLisp :: Bool
                         } deriving Show

defaultOptions :: Options
defaultOptions = Options { optToLisp = False
                         }

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option ['l'] ["tolisp"]
            (NoArg (\opts -> opts { optToLisp = True }))
            "print as a list of Lisp"
          ]

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> error (concat errs ++ usageInfo usage argspec)

----------------------------------------------------------------

main :: IO ()
main = flip catch handler $ do
    args <- getArgs
    let (opt,cmdArg) = parseArgs argspec args
        transform = if optToLisp opt then toLisp else toPlain
    ll <- case cmdArg !! 0 of
            cmd | cmd == "browse" -> browseModule $ cmdArg !! 1
                | cmd == "list"   -> listModules
            _                     -> error usage
    putStr $ transform $ nub $ sort $ ll
  where
    handler :: ErrorCall -> IO ()
    handler _ = putStrLn usage

----------------------------------------------------------------

browseModule :: String -> IO [String]
browseModule mname = do
    xs <- getSyntax mname
    let ys = preprocess xs
    return $ parseSyntax ys

getSyntax :: String -> IO String
getSyntax mname  = do
    (inp,out,_,_) <- runInteractiveProcess "ghci" [] Nothing Nothing
    mapM_ setFD [inp,out]
    hPutStrLn inp ":set prompt \"\""
    hPutStrLn inp "1"
    hPutStrLn inp $ ":browse " ++ mname
    hPutStrLn inp ":set prompt \"Prelude>\""
    hPutStrLn inp ":quit"
    cs <- hGetContents out
    return $ unlines $ dropTailer $ dropHeader $ lines $ cs
  where
    isNotPrefixOf x y = not (x `isPrefixOf` y)
    dropHeader xs = tail $ dropWhile (isNotPrefixOf "Prelude>") xs
    dropTailer = takeWhile (isNotPrefixOf "Prelude>")
    setFD h = do
        hSetBinaryMode h False
        hSetBuffering h LineBuffering

parseSyntax :: String -> [String]
parseSyntax xs = do
    let mode = defaultParseMode { extensions = NewQualifiedOperators : ExplicitForall : glasgowExts }
        res = parseModuleWithMode mode xs
    case res of
      ParseOk x       -> identifiers x
--      e               -> error $ show e
      _               -> []

----------------------------------------------------------------

listModules :: IO [String]
listModules = do
  cs <- getDump
  return $ exposedModules cs

getDump :: IO String
getDump = do
  (_,hout,_,_) <- runInteractiveProcess "ghc-pkg" ["dump"] Nothing Nothing
  hGetContents hout

exposedModules :: String -> [String]
exposedModules cs = let ls = unfoldLines cs
                        ns = values "name: " ls
                        ms = values "exposed-modules: " ls
                        zs = zip ns ms
                        xs = filter (\(nm,_) -> nm `notElem` ["ghc", "ghc-prim", "rts", "integer"]) zs
                        ss = map snd xs
                    in filter (\x -> not ("GHC" `isPrefixOf` x)) $ concatMap words ss

values :: String -> [String] -> [String]
values tag ls = let len = length tag
                    fs = filter (tag `isPrefixOf`) ls
                in map (drop len) fs

----------------------------------------------------------------

unfoldLines :: String -> [String]
unfoldLines xs = self xs
    where
      splitNL = break (== '\n')
      self "" = []
      self s  = let (l, s') = splitNL s
                in case s' of
                     []          -> [l]
                     (_:' ':s'') -> cont s'' l
                     (_:s'')     -> l : self s''
      cont s a = let (l, s') = splitNL $ dropWhile (== ' ') s
                     a' = a ++ " " ++ l
                 in case s' of
                      []          -> [a']
                      (_:' ':s'') -> cont s'' a'
                      (_:s'')     -> a' : self s''

toLisp :: [String] -> String
toLisp ms = "(" ++ unwords quoted ++ ")\n"
    where
      quote x = "\"" ++ x ++ "\""
      quoted = map quote ms

toPlain :: [String] -> String
toPlain = unlines

----------------------------------------------------------------

preprocess :: String -> String
preprocess cs = case parse remove "remove" cs of
                  Right a -> a
                  Left  e -> error $ show e

modName :: Parser String
modName = do c <- oneOf ['A'..'Z']
             cs <- many1 $ oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_'#"
             return $ c:cs

anyName :: Parser String
anyName = many1 $ oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_'#"

manyBefore :: Show tok => GenParser tok st a -> GenParser tok st [tok] -> GenParser tok st [a]
manyBefore p anchor = manyTill p (eof <|> try anc)
    where
      anc = do pos <- getPosition
               s <- anchor
               ss <- getInput
               setInput $ s ++ ss
               setPosition pos
               return ()

keyword :: Parser String
keyword = do ms <- modName
             char '.'
             return $ ms ++ ['.']

ghcName :: Parser String
ghcName = do keyword
             try sep <|> end
  where
   sep = do
       ws <- sepBy1 anyName (char '.')
       return $ last ws
   end = do
       endBy1 anyName (char '.')
       return ""

nonGhcName :: Parser String
nonGhcName = do c <- anyChar  -- making this func non-empty
                cs <- manyBefore anyChar keyword
                return $ c:cs

remove :: Parser String
remove = do l1 <- try ghcName <|> return ""
            l2 <- nonGhcName
            ll <- many (do x <- ghcName
                           y <- nonGhcName
                           return $ x ++ y)
            return $ concat $ l1 : l2 : ll

----------------------------------------------------------------

identifiers :: Module -> [String]
identifiers (Module _ _ _ _ _ _ x) = filter hid $ concatMap decl x
  where
    hid = all (\c -> isAlphaNum c || elem c "_'")

decl :: Decl -> [String]
decl (TypeSig _ [x] _) = [name x]
decl (DataDecl _ _ _ x _ y _) = name x : (map qualConDecl y)
decl (ClassDecl _ _ x _ _ y) = name x : (map classDecl y)
decl (TypeDecl _ x _ _) = [name x]
decl x = [show x]

qualConDecl :: QualConDecl -> String
qualConDecl (QualConDecl _ _ _ x) = conDecl x

conDecl :: ConDecl -> String
conDecl (ConDecl (Ident x) _) = x
conDecl (InfixConDecl _ (Ident x) _) = x
conDecl x = show x

classDecl :: ClassDecl -> String
classDecl (ClsDecl x) = concat $ decl x -- xxx
classDecl x = show x

name :: Name -> String
name (Symbol x) = x
name (Ident x) = x
