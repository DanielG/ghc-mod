module Dir (
    module Dir
  , getCurrentDirectory
  , (</>)
  ) where

import Control.Exception as E
import Data.List (isPrefixOf)
import System.Directory
import System.FilePath (addTrailingPathSeparator,(</>))



withDirectory_ :: FilePath -> IO a -> IO a
withDirectory_ dir action = bracket getCurrentDirectory
                                    setCurrentDirectory
                                    (\_ -> setCurrentDirectory dir >> action)

withDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withDirectory dir action = bracket getCurrentDirectory
                                   setCurrentDirectory
                                   (\d -> setCurrentDirectory dir >> action d)

toRelativeDir :: FilePath -> FilePath -> FilePath
toRelativeDir dir file
  | dir' `isPrefixOf` file = drop len file
  | otherwise              = file
  where
    dir' = addTrailingPathSeparator dir
    len = length dir'
