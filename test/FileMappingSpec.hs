module FileMappingSpec where

import GhcMod.FileMapping
import GhcMod.Utils (withMappedFile)
import Test.Hspec
import TestUtils
import qualified Data.Map as M
import Dir
import System.IO.Temp
import System.Directory

import GhcMod

spec :: Spec
spec = do
      describe "loadMappedFile" $ do
        it "inserts a given FilePath FileMapping into state with canonicalized path" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFile "File.hs" "File.hs"
              getMMappedFiles
            dir <- getCurrentDirectory
            show mappedFiles `shouldBe` show (M.fromList [(dir </> "File.hs", FileMapping "File.hs" False)])
        it "should try to guess a canonical name if file doesn't exist" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFile "NonExistantFile.hs" "File.hs"
              getMMappedFiles
            dir <- getCurrentDirectory
            show mappedFiles `shouldBe` show (M.fromList [(dir </> "NonExistantFile.hs", FileMapping "File.hs" False)])

      describe "loadMappedFileSource" $ do
        it "inserts a given FilePath FileMapping into state with canonicalized path" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFileSource "File.hs" "main :: IO ()"
              getMMappedFiles
            dir <- getCurrentDirectory
            -- TODO
            M.toList mappedFiles `shouldSatisfy` \[(fn, FileMapping _to True)] ->
              fn == dir </> "File.hs"
        it "should try to guess a canonical name if file doesn't exist" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFileSource "NonExistantFile.hs" "main :: IO ()"
              getMMappedFiles
            dir <- getCurrentDirectory
            -- TODO
            M.toList mappedFiles `shouldSatisfy` \[(fn, FileMapping _to True)] ->
              fn == dir </> "NonExistantFile.hs"

      describe "unloadMappedFile" $ do
        it "removes a given FilePath from state" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFile "File.hs" "File2.hs"
              unloadMappedFile "File.hs"
              getMMappedFiles
            show mappedFiles `shouldBe` show (M.fromList ([] :: [(FilePath, FileMapping)]))
        it "should work even if file does not exist" $ do
          withDirectory_ "test/data/file-mapping" $ do
            mappedFiles <- runD $ do
              loadMappedFile "NonExistantFile.hs" "File2.hs"
              unloadMappedFile "NonExistantFile.hs"
              getMMappedFiles
            show mappedFiles `shouldBe` show (M.fromList ([] :: [(FilePath, FileMapping)]))
        it "should remove created temporary files" $ do
          withDirectory_ "test/data/file-mapping" $ do
            dir <- getCurrentDirectory
            fileExists <- runD $ do
              loadMappedFileSource "NonExistantFile.hs" "main :: IO ()"
              fp <- maybe undefined fmPath `fmap` lookupMMappedFile (dir </> "NonExistantFile.hs")
              unloadMappedFile "NonExistantFile.hs"
              liftIO $ doesFileExist fp
            not fileExists `shouldBe` True

      describe "withMappedFile" $ do
        it "checks if there is a redirected file and calls and action with its FilePath" $ do
          withDirectory_ "test/data/file-mapping" $ do
            res <- runD $ do
              loadMappedFile "File.hs" "File_Redir.hs"
              withMappedFile "File.hs" return
            res `shouldBe` "File_Redir.hs"
        it "checks if there is an in-memory file and calls and action with temporary file" $ do
          withDirectory_ "test/data/file-mapping" $ do
            (fn, src) <- runD $ do
              loadMappedFileSource "File.hs" "main = test"
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
            let fm = [("File.hs", "File_Redir.hs")]
            res <- run defaultOptions $ do
              mapM_ (uncurry loadMappedFile) fm
              checkSyntax ["File.hs"]
            res `shouldBe` "File.hs:1:1:Warning: Top-level binding with no type signature: main :: IO ()\n"
        it "checks in-memory file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping" $ do
            let fm = [("File.hs", "main = putStrLn \"Hello World!\"\n")]
            res <- run defaultOptions $ do
              mapM_ (uncurry loadMappedFileSource) fm
              checkSyntax ["File.hs"]
            res `shouldBe` "File.hs:1:1:Warning: Top-level binding with no type signature: main :: IO ()\n"
        it "should work even if file doesn't exist" $ do
          withDirectory_ "test/data/file-mapping" $ do
            let fm = [("Nonexistent.hs", "main = putStrLn \"Hello World!\"\n")]
            res <- run defaultOptions $ do
              mapM_ (uncurry loadMappedFileSource) fm
              checkSyntax ["Nonexistent.hs"]
            res `shouldBe` "Nonexistent.hs:1:1:Warning: Top-level binding with no type signature: main :: IO ()\n"
        it "lints redirected file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping" $ do
            res <- runD $ do
              loadMappedFile "File.hs" "File_Redir_Lint.hs"
              lint lintOpts "File.hs"
            res `shouldBe` "File.hs:4:1: Warning: Eta reduce\NULFound:\NUL  func a b = (*) a b\NULWhy not:\NUL  func = (*)\n"
        it "lints in-memory file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping" $ do
            res <- runD $ do
              loadMappedFileSource "File.hs" "func a b = (++) a b\n"
              lint lintOpts "File.hs"
            res `shouldBe` "File.hs:1:1: Warning: Eta reduce\NULFound:\NUL  func a b = (++) a b\NULWhy not:\NUL  func = (++)\n"
        it "shows types of the expression for redirected files" $ do
            let tdir = "test/data/file-mapping"
            res <- runD' tdir $ do
              loadMappedFile "File.hs" "File_Redir_Lint.hs"
              types False "File.hs" 4 12
            res `shouldBe` "4 12 4 15 \"a -> a -> a\"\n4 12 4 17 \"a -> a\"\n4 12 4 19 \"a\"\n4 1 4 19 \"a -> a -> a\"\n"
        it "shows types of the expression with constraints for redirected files" $ do --
            let tdir = "test/data/file-mapping"
            res <- runD' tdir $ do
              loadMappedFile "File.hs" "File_Redir_Lint.hs"
              types True "File.hs" 4 12
            res `shouldBe` "4 12 4 15 \"a -> a -> a\"\n4 12 4 17 \"a -> a\"\n4 12 4 19 \"a\"\n4 1 4 19 \"Num a => a -> a -> a\"\n"
        it "shows types of the expression for in-memory files" $ do
            let tdir = "test/data/file-mapping"
            res <- runD' tdir $ do
              loadMappedFileSource "File.hs" "main = putStrLn \"Hello!\""
              types False "File.hs" 1 14
            res `shouldBe` "1 8 1 16 \"String -> IO ()\"\n1 8 1 25 \"IO ()\"\n1 1 1 25 \"IO ()\"\n"
        it "shows info for the expression for redirected files" $ do
            let tdir = "test/data/file-mapping"
            res <- runD' tdir $ do
              loadMappedFile "File.hs" "File_Redir_Lint.hs"
              info "File.hs" $ Expression "func"
            res `shouldBe` "func :: Num a => a -> a -> a \t-- Defined at File.hs:4:1\n"
        it "shows info for the expression for in-memory files" $ do
            let tdir = "test/data/file-mapping"
            res <- runD' tdir $ do
              loadMappedFileSource "File.hs" "module File where\n\ntestfun = putStrLn \"Hello!\""
              info "File.hs" $ Expression "testfun"
            res `shouldBe` "testfun :: IO () \t-- Defined at File.hs:3:1\n"

      describe "preprocessor tests" $ do
        it "checks redirected file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping/preprocessor" $ do
            let fm = [("File.hs", "File_Redir.hs")]
            res <- run defaultOptions $ do
              mapM_ (uncurry loadMappedFile) fm
              checkSyntax ["File.hs"]
            res `shouldBe` "File.hs:3:1:Warning: Top-level binding with no type signature: main :: IO ()\n"
        it "works with full path as well" $ do
          withDirectory_ "test/data/file-mapping/preprocessor" $ do
            cwd <- getCurrentDirectory
            let fm = [("File.hs", cwd </> "File_Redir.hs")]
            res <- run defaultOptions $ do
              mapM_ (uncurry loadMappedFile) fm
              checkSyntax ["File.hs"]
            res `shouldBe` "File.hs:3:1:Warning: Top-level binding with no type signature: main :: IO ()\n"
        it "checks in-memory file" $ do
          withDirectory_ "test/data/file-mapping/preprocessor" $ do
            src <- readFile "File_Redir.hs"
            let fm = [("File.hs", src)]
            res <- run defaultOptions $ do
              mapM_ (uncurry loadMappedFileSource) fm
              checkSyntax ["File.hs"]
            res `shouldBe` "File.hs:3:1:Warning: Top-level binding with no type signature: main :: IO ()\n"
        it "lints redirected file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping/preprocessor" $ do
            res <- runD $ do
              loadMappedFile "File.hs" "File_Redir_Lint.hs"
              lint lintOpts "File.hs"
            res `shouldBe` "File.hs:6:1: Warning: Eta reduce\NULFound:\NUL  func a b = (*) a b\NULWhy not:\NUL  func = (*)\n"
        it "lints in-memory file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping/preprocessor" $ do
            src <- readFile "File_Redir_Lint.hs"
            res <- runD $ do
              loadMappedFileSource "File.hs" src
              lint lintOpts "File.hs"
            res `shouldBe` "File.hs:6:1: Warning: Eta reduce\NULFound:\NUL  func a b = (*) a b\NULWhy not:\NUL  func = (*)\n"
      describe "literate haskell tests" $ do
        it "checks redirected file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping/lhs" $ do
            let fm = [("File.lhs", "File_Redir.lhs")]
            res <- run defaultOptions $ do
              mapM_ (uncurry loadMappedFile) fm
              checkSyntax ["File.lhs"]
            res `shouldBe` "File.lhs:1:3:Warning: Top-level binding with no type signature: main :: IO ()\n"
        it "checks in-memory file if one is specified and outputs original filename" $ do
          withDirectory_ "test/data/file-mapping/lhs" $ do
            src <- readFile "File_Redir.lhs"
            let fm = [("File.lhs", src)]
            res <- run defaultOptions $ do
              mapM_ (uncurry loadMappedFileSource) fm
              checkSyntax ["File.lhs"]
            res `shouldBe` "File.lhs:1:3:Warning: Top-level binding with no type signature: main :: IO ()\n"
        -- NOTE: There is a bug in hlint that prevents it from linting lhs files.
        -- it "lints redirected file if one is specified and outputs original filename" $ do
        --   withDirectory_ "test/data/file-mapping/lhs" $ do
        --     res <- runD $ do
        --       loadMappedFile "File.lhs" (RedirectedMapping "File_Redir_Lint.lhs")
        --       lint "File.lhs"
        --     res `shouldBe` "File.lhs:6:1: Error: Eta reduce\NULFound:\NUL  func a b = (*) a b\NULWhy not:\NUL  func = (*)\n"
        -- it "lints in-memory file if one is specified and outputs original filename" $ do
        --   withDirectory_ "test/data/file-mapping/lhs" $ do
        --     src <- readFile "File_Redir_Lint.lhs"
        --     res <- runD $ do
        --       loadMappedFile "File.lhs" (MemoryMapping $ Just src)
        --       lint "File.lhs"
        --     res `shouldBe` "File.lhs:6:1: Error: Eta reduce\NULFound:\NUL  func a b = (*) a b\NULWhy not:\NUL  func = (*)\n"
      describe "template haskell" $ do
        it "works with a redirected module using TemplateHaskell" $ do
          withSystemTempDirectory "ghc-mod-test" $ \tmpdir -> do
            srcFoo <- readFile "test/data/template-haskell/Foo.hs"
            srcBar <- readFile "test/data/template-haskell/Bar.hs"
            withDirectory_ "test/data/file-mapping" $ do
              writeFile (tmpdir </> "Foo_Redir.hs") srcFoo
              writeFile (tmpdir </> "Bar_Redir.hs") srcBar
              let fm = [("Foo.hs", tmpdir </> "Foo_Redir.hs")
                       ,("Bar.hs", tmpdir </> "Bar_Redir.hs")]
              res <- run defaultOptions $ do
                mapM_ (uncurry loadMappedFile) fm
                types False "Bar.hs" 5 1
              res `shouldBe` unlines ["5 1 5 20 \"[Char]\""]
        it "works with a memory module using TemplateHaskell" $ do
          srcFoo <- readFile "test/data/template-haskell/Foo.hs"
          srcBar <- readFile "test/data/template-haskell/Bar.hs"
          withDirectory_ "test/data/file-mapping" $ do
            let fm = [("Foo.hs", srcFoo)
                     ,("Bar.hs", srcBar)]
            res <- run defaultOptions $ do
              mapM_ (uncurry loadMappedFileSource) fm
              types False "Bar.hs" 5 1
            res `shouldBe` unlines ["5 1 5 20 \"[Char]\""]


lintOpts :: LintOpts
lintOpts =
    defaultLintOpts { optLintHlintOpts = ["--ignore=Use module export list"] }
