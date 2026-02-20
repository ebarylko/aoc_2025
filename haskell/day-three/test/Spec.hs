import Test.Hspec

import DayThree (mkBattery, findMstPotentBattery)

main :: IO ()
main = hspec $ do
  describe "Finding the most potent battery in a bank" $ do
    describe "Given a collection of batteries where the third element has the highest voltage" $ do
      it "The third element and its position is returned" $ do
        let sample = map mkBattery [1, 2, 3]

        let expected = (2, mkBattery 3)
        findMstPotentBattery sample `shouldBe` expected
