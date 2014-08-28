module UtilsSpec where

import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Utils
import TestUtils
import Test.Hspec

spec :: Spec
spec = do
    describe "extractParens" $ do
        it "extracts the part of a string surrounded by parentheses" $ do
            extractParens "asdasdasd ( hello [ world ] )()() kljlkjlkjlk" `shouldBe` "( hello [ world ] )"
            extractParens "[(PackageName \"template-haskell\",InstalledPackageId \"template-haskell-2.9.0.0-8e2a49468f3b663b671c437d8579cd28\"),(PackageName \"base\",InstalledPackageId \"base-4.7.0.0-e4567cc9a8ef85f78696b03f3547b6d5\"),(PackageName \"Cabal\",InstalledPackageId \"Cabal-1.18.1.3-b9a44a5b15a8bce47d40128ac326e369\")][][]" `shouldBe` "[(PackageName \"template-haskell\",InstalledPackageId \"template-haskell-2.9.0.0-8e2a49468f3b663b671c437d8579cd28\"),(PackageName \"base\",InstalledPackageId \"base-4.7.0.0-e4567cc9a8ef85f78696b03f3547b6d5\"),(PackageName \"Cabal\",InstalledPackageId \"Cabal-1.18.1.3-b9a44a5b15a8bce47d40128ac326e369\")]"

    describe "liftMonadError" $ do
        it "converts IOErrors to GhcModError" $ do
            shouldReturnError $
                runD' $ liftIO $ throw (userError "hello") >> return ""

            shouldReturnError $
                runD' $ liftIO $ readFile "/DOES_NOT_EXIST" >> return ""

-- readProcessWithExitCode cmd opts ""
