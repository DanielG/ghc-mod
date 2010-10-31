{-# LANGUAGE FlexibleContexts #-}
module GetType (getType) where

import Control.Applicative hiding (many, (<|>))
import Language.Haskell.Interpreter (runInterpreter, typeOf, setImportsQ)
import Text.Parsec

type ImportList = [(String, [Maybe String])]

imports, importList :: (Stream s m Char) => ParsecT s u m ImportList
imports = char '(' *> importList <* char ')'

importList = sepBy importModule spaces

qualifiedName :: (Stream s m Char) => ParsecT s u m (Maybe String)
qualifiedName = (Just <$> elispString) <|> (string "nil" *> return Nothing)

elispString :: (Stream s m Char) => ParsecT s u m String
elispString = between (char '\"') (char '\"') (many1 (alphaNum <|> char '.'))

importModule :: (Stream s m Char) => ParsecT s u m (String, [Maybe String])
importModule = do
  mod' <- char '(' *> elispString
  many1 space
  qualifiedNames <- between (char '(') (char ')') stringList
  char ')'
  return (mod', qualifiedNames)
  where stringList = sepBy1 qualifiedName spaces

getType :: String -> String -> IO String
getType sym mods = fmap (either (const "") id) . runInterpreter $ do
  let imported = either (const []) id . parse imports "none" $ mods
  setImportsQ . concatMap toHintImport $ imported
  typeOf sym
  where toHintImport :: (String, [Maybe String]) -> [(String, Maybe String)]
        toHintImport (name, quals) = [(name, q) | q <- quals]
