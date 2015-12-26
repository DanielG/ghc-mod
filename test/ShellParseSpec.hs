module ShellParseSpec where


import GHCMod.Options.ShellParse

import Test.Hspec

spec :: Spec
spec =
  describe "parseCmdLine" $ do
    it "splits arguments" $
      parseCmdLine "test command line" `shouldBe` ["test", "command", "line"]
    it "honors quoted segments" $
      parseCmdLine "test command line \STXwith quoted segment\ETX"
        `shouldBe` ["test", "command", "line", "with quoted segment"]
    it "squashes multiple spaces" $
      parseCmdLine "test       command"
        `shouldBe` ["test", "command"]
