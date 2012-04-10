{-# LANGUAGE FlexibleInstances #-}

module Types where

data OutputStyle = LispStyle | PlainStyle

data Options = Options {
    outputStyle  :: OutputStyle
  , hlintOpts    :: [String]
  , ghcOpts      :: [String]
  , operators    :: Bool
  , expandSplice :: Bool
  , sandbox      :: Maybe String
  }

defaultOptions :: Options
defaultOptions = Options {
    outputStyle  = PlainStyle
  , hlintOpts    = []
  , ghcOpts      = []
  , operators    = False
  , expandSplice = False
  , sandbox      = Nothing
  }

----------------------------------------------------------------

convert :: ToString a => Options -> a -> String
convert Options{ outputStyle = LispStyle  } = toLisp
convert Options{ outputStyle = PlainStyle } = toPlain

class ToString a where
    toLisp  :: a -> String
    toPlain :: a -> String

instance ToString [String] where
    toLisp  = addNewLine . toSexp True
    toPlain = unlines

instance ToString [((Int,Int,Int,Int),String)] where
    toLisp  = addNewLine . toSexp False . map toS
      where
        toS x = "(" ++ tupToString x ++ ")"
    toPlain = unlines . map tupToString

toSexp :: Bool -> [String] -> String
toSexp False ss = "(" ++ unwords ss ++ ")"
toSexp True ss  = "(" ++ unwords (map quote ss) ++ ")"

tupToString :: ((Int,Int,Int,Int),String) -> String
tupToString ((a,b,c,d),s) = show a ++ " "
                         ++ show b ++ " "
                         ++ show c ++ " "
                         ++ show d ++ " "
                         ++ quote s

quote :: String -> String
quote x = "\"" ++ x ++ "\""

addNewLine :: String -> String
addNewLine = (++ "\n")
