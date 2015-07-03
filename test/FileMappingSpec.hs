module FileMappingSpec where

import Language.Haskell.GhcMod.FileMapping
import Language.Haskell.GhcMod.Utils (withMappedFile)
import Test.Hspec
import TestUtils
import qualified Data.Map as M
import Dir
import Data.Maybe

import Language.Haskell.GhcMod

spec :: Spec
spec = do
      describe "loadMappedFile" $ do
        it "inserts a given FilePath FileMapping into state with canonicalized path" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFile "File.hs" (MemoryMapping Nothing)
              getMMappedFiles
            dir <- getCurrentDirectory
            show mappedFiles `shouldBe` show (M.fromList [(dir </> "File.hs", MemoryMapping Nothing)])
        it "should try to guess a canonical name if file doesn't exist" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFile "NonExistantFile.hs" (MemoryMapping Nothing)
              getMMappedFiles
            dir <- getCurrentDirectory
            show mappedFiles `shouldBe` show (M.fromList [(dir </> "NonExistantFile.hs", MemoryMapping Nothing)])

      describe "unloadMappedFile" $ do
        it "removes a given FilePath from state" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFile "File.hs" (MemoryMapping Nothing)
              unloadMappedFile "File.hs"
              getMMappedFiles
            show mappedFiles `shouldBe` show (M.fromList ([] :: [(FilePath, FileMapping)]))
        it "should work even if file does not exist" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFile "NonExistantFile.hs" (MemoryMapping Nothing)
              unloadMappedFile "NonExistantFile.hs"
              getMMappedFiles
            show mappedFiles `shouldBe` show (M.fromList ([] :: [(FilePath, FileMapping)]))

      describe "loadMappedFiles" $ do
        it "loads all file mappings passed as Options" $ do
          let fm = [("File.hs", RedirectedMapping "File_Redir.hs"), ("File2.hs", MemoryMapping Nothing)]
          mappedFiles <- run defaultOptions { fileMappings = fm } $
            loadMappedFiles >> getMMappedFiles
          dir <- getCurrentDirectory
          M.lookup (dir </> "File.hs")  mappedFiles `shouldSatisfy` isJust
          M.lookup (dir </> "File2.hs") mappedFiles `shouldSatisfy` isJust
        it "prioritizes latter occurence of the same file" $ do
          let fm = [("File.hs", RedirectedMapping "File_Redir.hs"), ("File.hs", MemoryMapping Nothing)]
          mappedFiles <- run defaultOptions { fileMappings = fm } $
            loadMappedFiles >> getMMappedFiles
          dir <- getCurrentDirectory
          show (M.lookup (dir </> "File.hs")  mappedFiles) `shouldBe` show (Just (MemoryMapping Nothing))

      describe "withMappedFile" $ do
        it "checks if there is a redirected file and calls and action with its FilePath" $ do
          withDirectory_ "test/data/file-mapping" $ do
            res <- runD $ do
              loadMappedFile "File.hs" (RedirectedMapping "File_Redir.hs")
              withMappedFile "File.hs" return
            res `shouldBe` "File_Redir.hs"
        it "checks if there is an in-memory file and calls and action with temporary file" $ do
          withDirectory_ "test/data/file-mapping" $ do
            (fn, src) <- runD $ do
              loadMappedFile "File.hs" (MemoryMapping $ Just "main = test")
              withMappedFile "File.hs" $ \fn -> do
                src <- liftIO $ readFile fn
                return (fn, src)
            fn `shouldSatisfy` (/="File.hs")
            src `shouldBe` "main = test"
        it "runs action with original filename if there is no mapping" $ do
          withDirectory_ "test/data/file-mapping" $ do
            fn <- runD $ do
              withMappedFile "File.hs" return
            fn `shouldBe` "File.hs"

      describe "integration tests" $ do
        it "checks redirected file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping" $ do
            let fm = [("File.hs", RedirectedMapping "File_Redir.hs")]
            res <- run defaultOptions {fileMappings = fm} $ do
              loadMappedFiles
              checkSyntax ["File.hs"]
            res `shouldBe` "File.hs:1:1:Warning: Top-level binding with no type signature: main :: IO ()\n"
        it "checks in-memory file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping" $ do
            let fm = [("File.hs", MemoryMapping $ Just "main = putStrLn \"Hello World!\"\n")]
            res <- run defaultOptions {fileMappings = fm} $ do
              loadMappedFiles
              checkSyntax ["File.hs"]
            res `shouldBe` "File.hs:1:1:Warning: Top-level binding with no type signature: main :: IO ()\n"
        it "lints redirected file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping" $ do
            res <- runD $ do
              loadMappedFile "File.hs" (RedirectedMapping "File_Redir_Lint.hs")
              lint "File.hs"
            res `shouldBe` "File.hs:4:1: Error: Eta reduce\NULFound:\NUL  func a b = (*) a b\NULWhy not:\NUL  func = (*)\n"
        it "lints in-memory file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping" $ do
            res <- runD $ do
              loadMappedFile "File.hs" (MemoryMapping $ Just "func a b = (++) a b\n")
              lint "File.hs"
            res `shouldBe` "File.hs:1:1: Error: Eta reduce\NULFound:\NUL  func a b = (++) a b\NULWhy not:\NUL  func = (++)\n"
        it "shows types of the expression for redirected files" $ do
            let tdir = "test/data/file-mapping"
            res <- runD' tdir $ do
              loadMappedFile "File.hs" (RedirectedMapping "File_Redir_Lint.hs")
              types "File.hs" 4 12
            res `shouldBe` "4 12 4 15 \"a -> a -> a\"\n4 12 4 17 \"a -> a\"\n4 12 4 19 \"a\"\n4 1 4 19 \"a -> a -> a\"\n"
        it "shows types of the expression for in-memory files" $ do
            let tdir = "test/data/file-mapping"
            res <- runD' tdir $ do
              loadMappedFile "File.hs" (MemoryMapping $ Just "main = putStrLn \"Hello!\"")
              types "File.hs" 1 14
            res `shouldBe` "1 8 1 16 \"String -> IO ()\"\n1 8 1 25 \"IO ()\"\n1 1 1 25 \"IO ()\"\n"
        it "shows info for the expression for redirected files" $ do
            let tdir = "test/data/file-mapping"
            res <- runD' tdir $ do
              loadMappedFile "File.hs" (RedirectedMapping "File_Redir_Lint.hs")
              info "File.hs" $ Expression "func"
            res `shouldBe` "func :: Num a => a -> a -> a \t-- Defined at File.hs:4:1\n"
        it "shows info for the expression for in-memory files" $ do
            let tdir = "test/data/file-mapping"
            res <- runD' tdir $ do
              loadMappedFile "File.hs" (MemoryMapping $ Just "module File where\n\ntestfun = putStrLn \"Hello!\"")
              info "File.hs" $ Expression "testfun"
            res `shouldBe` "testfun :: IO () \t-- Defined at File.hs:3:1\n"
