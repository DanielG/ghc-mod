module Expectation where

import Test.Hspec

shouldContain :: Eq a => [a] -> a -> Expectation
shouldContain containers element = do
    let res = element `elem` containers
    res `shouldBe` True
