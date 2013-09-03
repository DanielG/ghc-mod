module Dir where

import System.Directory
import Control.Exception as E

withDirectory_ :: FilePath -> IO a -> IO a
withDirectory_ dir action = bracket getCurrentDirectory
                                    setCurrentDirectory
                                    (\_ -> setCurrentDirectory dir >> action)

withDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withDirectory dir action = bracket getCurrentDirectory
                                   setCurrentDirectory
                                   (\d -> setCurrentDirectory dir >> action d)
