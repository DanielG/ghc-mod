{-# LANGUAGE ScopedTypeVariables #-}
module MonadSpec where

import Test.Hspec
import Dir
import TestUtils
import Control.Applicative
import Control.Monad.Error.Class
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Find

spec :: Spec
spec = do
    describe "When using GhcModT in a do block" $
        it "a pattern match failure causes a call to `fail` on ErrorT in the monad stack" $ do
             (a, w)
                 <- runGhcModT defaultOptions $
                       do
                         Just a <- return Nothing
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
