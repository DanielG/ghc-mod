module Expectation where

import Test.Hspec
import System.Directory
import Control.Exception as E

shouldContain :: Eq a => [a] -> a -> Expectation
shouldContain containers element = do
    let res = element `elem` containers
    res `shouldBe` True

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir action = bracket getCurrentDirectory
                                   setCurrentDirectory
                                   (\_ -> setCurrentDirectory dir >> action)
