{-# LANGUAGE ScopedTypeVariables #-}
module MonadSpec where

import Test.Hspec
import Dir
import TestUtils
import Control.Applicative
import Control.Exception
import Control.Monad.Error.Class

spec :: Spec
spec = do
    describe "When using GhcModT in a do block" $
        it "a pattern match failure causes a call to `fail` on ErrorT in the monad stack" $ do
             (a, _)
                 <- runGhcModT defaultOptions $
                       do
                         Just _ <- return Nothing
                         return "hello"
                     `catchError` (const $ fail "oh noes")
             a `shouldBe` (Left $ GMEString "oh noes")

    describe "runGhcModT" $
        it "complains if the cabal file fails to parse while a sandbox is present" $ withDirectory_ "test/data/broken-cabal" $ do
          shouldReturnError $ runD' (gmCradle <$> ask)

    describe "gmsGet/Put" $
        it "work" $ do
          (runD $ gmsPut (GhcModState Intelligent) >> gmsGet)
            `shouldReturn` (GhcModState Intelligent)

    describe "liftIO" $ do
        it "converts user errors to GhcModError" $ do
            shouldReturnError $
                runD' $ liftIO $ throw (userError "hello") >> return ""

        it "converts a file not found exception to GhcModError" $ do
            shouldReturnError $
                runD' $ liftIO $ readFile "/DOES_NOT_EXIST" >> return ""
