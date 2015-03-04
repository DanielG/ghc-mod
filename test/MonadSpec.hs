module MonadSpec where

import Test.Hspec
import TestUtils
import Control.Monad.Error.Class

spec :: Spec
spec = do
    describe "When using GhcModT in a do block" $
        it "a pattern match failure causes a call to `fail` on ErrorT in the monad stack" $ do
             (a, _h)
                 <- runGhcModT defaultOptions $
                       do
                         Just _ <- return Nothing
                         return "hello"
                     `catchError` (const $ fail "oh noes")
             a `shouldBe` (Left $ GMEString "oh noes")
