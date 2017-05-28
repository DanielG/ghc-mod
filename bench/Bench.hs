import Criterion.Main
import GhcMod.Target
import GhcMod.Monad
import GhcMod.Types
import Dir
import System.IO.Temp
import System.Process hiding (env)
import Control.Monad

main = defaultMain [
    env setup $ \dir -> bgroup "simple-cabal" [
        bench "nop" $ whnfIO (simpleCabalNop dir 1)
      , bench "nop10" $ whnfIO (simpleCabalNop dir 10)
      ]
  ]

setup = do
  tdir <- createTempDirectory "/tmp" "ghc-mod-bench"
  system $ "cp -rv \"bench/data/simple-cabal/\" \""++ tdir ++"\""

  simpleCabalNop tdir 1 -- warmup dist/

  return tdir

simpleCabalNop :: FilePath -> Int -> IO ()
simpleCabalNop dir n = withDirectory_ (dir </> "simple-cabal") $ do
  _ <- runGhcModT defaultOptions $
       forM_ [1..n] $ \_ -> do
           runGmlT [Left "Main.hs"] (return ())
  return ()
