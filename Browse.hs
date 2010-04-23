module Browse (browseModule) where

import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser hiding (parse)
import Language.Haskell.Exts.Syntax
import System.IO
import System.Process
import Text.Parsec
import Text.Parsec.String
import Param

----------------------------------------------------------------

browseModule :: Options -> String -> IO String
browseModule opt mname = convert opt . nub . sort . parseSyntax . preprocess <$> getSyntax opt mname

getSyntax :: Options -> String -> IO String
getSyntax opt mname  = do
    (inp,out,_,_) <- runInteractiveProcess (ghci opt) [] Nothing Nothing
    mapM_ setFD [inp,out]
    hPutStrLn inp ":set prompt \"\""
    hPutStrLn inp "1"
    hPutStrLn inp $ ":browse " ++ mname
    hPutStrLn inp ":set prompt \"Prelude>\""
    hPutStrLn inp ":quit"
    cs <- hGetContents out
    return . unlines . dropTailer . dropHeader . lines $ cs
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
      _               -> []


----------------------------------------------------------------

preprocess :: String -> String
preprocess cs = case parse remove "remove" cs of
                  Right a -> a
                  Left  e -> error $ show e

modName :: Parser String
modName = (:) <$> oneOf ['A'..'Z']
              <*> (many . oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_'#")

anyName :: Parser String
anyName = many1 . oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_'#"

manyBefore :: Show tok => GenParser tok st a -> GenParser tok st [tok] -> GenParser tok st [a]
manyBefore p anchor = manyTill p (eof <|> try anc)
    where
      anc = do
          pos <- getPosition
          s <- anchor
          ss <- getInput
          setInput $ s ++ ss
          setPosition pos
          return ()

keyword :: Parser String
keyword = (++) <$> modName <*> string "."

ghcName :: Parser String
ghcName = do 
    keyword
    try sep <|> end
  where
   sep = last <$> sepBy1 anyName (char '.')
   end = "" <$ endBy1 anyName (char '.')

nonGhcName :: Parser String
nonGhcName = (:) <$> anyChar <*> manyBefore anyChar keyword

remove :: Parser String
remove = do
    l1 <- try ghcName <|> return ""
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
decl (DataDecl _ _ _ x _ y _) = name x : map qualConDecl y
decl (ClassDecl _ _ x _ _ y)  = name x : map classDecl y
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
