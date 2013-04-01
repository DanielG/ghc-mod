module DebugSpec where

import CabalApi
import Cradle
import Debug
import Expectation
import Test.Hspec
import Types

checkFast :: String -> String -> IO ()
checkFast file ans = withDirectory_ "test/data" $ do
    (strVer,_) <- getGHCVersion
    cradle <- findCradle Nothing strVer
    res <- debug defaultOptions cradle strVer file
    res `shouldContain` ans

spec :: Spec
spec = do
    describe "debug" $ do
        it "can check TH" $ do
            checkFast "Main.hs" "Fast check:          No"
            checkFast "Foo.hs"  "Fast check:          Yes"
            checkFast "Bar.hs"  "Fast check:          No"

        it "can check QuasiQuotes" $ do
            checkFast "Baz.hs"  "Fast check:          No"
