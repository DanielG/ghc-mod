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

          mv_ex :: MVar (Either SomeException ())
              <- newEmptyMVar
          mv_startup_barrier :: MVar ()
              <- newEmptyMVar

          _t1 <- forkOS $ do
                 -- wait (inside GhcModT) for t2 to receive the exception
                 _ <- runD $ liftIO $ do
                            putMVar mv_startup_barrier ()
                            readMVar mv_ex
                 return ()

          _t2 <- forkOS $ do
                 readMVar mv_startup_barrier -- wait for t1 to be in GhcModT
                 res <- try $ runD $ return ()
                 res' <- evaluate res
                 putMVar mv_ex res'

          ex <- takeMVar mv_ex

          isLeft ex `shouldBe` True

isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left _) = True
