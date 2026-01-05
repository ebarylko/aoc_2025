module Tests where

import Test.Hspec

main :: IO ()

main = hspec $ do
  describe "Running a test" $ do
    it "Should not fail" $ do
      pending
