module ShellEscapeSpec where


import GHCMod.Options.ShellEscape

import Test.Hspec

spec :: Spec
spec =
  describe "parseCmdLine" $ do
    it "splits arguments" $
      parseCmdLine "test command line" `shouldBe` ["test", "command", "line"]
    it "honors double quotes" $
      parseCmdLine "test command line \"with double quotes\""
        `shouldBe` ["test", "command", "line", "with double quotes"]
    it "honors single quotes" $
      parseCmdLine "test command line 'with single quotes'"
        `shouldBe` ["test", "command", "line", "with single quotes"]
    it "understands single quote in double quotes" $
      parseCmdLine "test for \"quoted argument with ' single quote\" here"
        `shouldBe` ["test", "for", "quoted argument with ' single quote", "here"]
    it "understands double quote in single quotes" $
      parseCmdLine "test for \'quoted argument with \" double quote\' here"
        `shouldBe` ["test", "for", "quoted argument with \" double quote", "here"]
    it "escapes spaces" $ do
      parseCmdLine "with\\ spaces"
        `shouldBe` ["with spaces"]
      parseCmdLine "'with\\ spaces'"
        `shouldBe` ["with spaces"]
      parseCmdLine "\"with\\ spaces\""
        `shouldBe` ["with spaces"]
    it "escapes '\\'" $ do
      parseCmdLine "\\\\"
        `shouldBe` ["\\"]
      parseCmdLine "\"\\\\\""
        `shouldBe` ["\\"]
      parseCmdLine "'\\\\'"
        `shouldBe` ["\\"]
    it "escapes single quotes" $ do
      parseCmdLine "\\'"
        `shouldBe` ["'"]
      parseCmdLine "'\\''"
        `shouldBe` ["'"]
      parseCmdLine "\"\\'\""
        `shouldBe` ["'"]
    it "escapes double quotes" $ do
      parseCmdLine "\\\""
        `shouldBe` ["\""]
      parseCmdLine "'\\\"'"
        `shouldBe` ["\""]
      parseCmdLine "\"\\\"\""
        `shouldBe` ["\""]
    it "doesn't escape random characters" $
      parseCmdLine "\\a\\b\\c"
        `shouldBe` ["\\a\\b\\c"]
    it "squashes multiple spaces" $
      parseCmdLine "test       command"
        `shouldBe` ["test", "command"]
