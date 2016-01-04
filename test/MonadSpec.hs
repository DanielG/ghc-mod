module MonadSpec where

import Test.Hspec
import TestUtils
import Control.Monad.Error.Class
import Control.Concurrent
import Control.Exception

spec :: Spec
spec = do
    describe "When using GhcModT in a do block" $
        it "a pattern match failure causes a call to `fail` on ErrorT in the monad stack" $ do
             (a, _h)
                 <- runGmOutDef $ runGhcModT defaultOptions $
                       do
                         Just _ <- return Nothing
                         return "hello"
                     `catchError` (const $ fail "oh noes")
             a `shouldBe` (Left $ GMEString "oh noes")

    describe "runGhcModT" $
        it "throws an exception when run in multiple threads" $ do
          mv1 :: MVar (Either SomeException ())
              <- newEmptyMVar
          mv2 :: MVar (Either SomeException ())
              <- newEmptyMVar

          _ <- forkOS $ putMVar mv1 =<< (try $ evaluate =<< (runD $ liftIO $ readMVar mv2 >> return ()))
          _ <- forkOS $ putMVar mv2 =<< (try $ evaluate =<< (runD $ return ()))
          e1 <- takeMVar mv1
          e2 <- takeMVar mv2

          (isLeft e1 || isLeft e2) `shouldBe` True

isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left _) = True
