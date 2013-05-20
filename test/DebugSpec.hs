module DebugSpec where

import Expectation
import Language.Haskell.GhcMod
import Test.Hspec

checkFast :: String -> String -> IO ()
checkFast file ans = withDirectory_ "test/data" $ do
    (strVer,_) <- getGHCVersion
    cradle <- findCradle Nothing strVer
    res <- debugInfo defaultOptions cradle strVer file
    lines res `shouldContain` ans

spec :: Spec
spec = do
    describe "debug" $ do
        it "can check TH" $ do
            checkFast "Main.hs" "Fast check:          No"
            checkFast "Foo.hs"  "Fast check:          Yes"
            checkFast "Bar.hs"  "Fast check:          No"

        it "can check QuasiQuotes" $ do
            checkFast "Baz.hs"  "Fast check:          No"
