import Test.Hspec

import DayThree (findMstPotentBattery, Battery(..), Joltage(..), calcBestJoltage, Bank)
import Data.Function ((&))
import Data.Char (digitToInt)
import Control.Arrow ((>>>))

-- Takes a number n in [1, 9] and creates a battery with a joltage of n
mkBattery :: Int -> Battery

mkBattery = Battery . Joltage

mkBank :: String -> Bank

mkBank = map (mkBattery . digitToInt)

addJoltages :: Joltage -> Joltage -> Joltage

addJoltages (Joltage a) (Joltage b) = Joltage $ a + b
sumJoltages = foldr addJoltages (Joltage 0)

main :: IO ()
main = hspec $ do
  describe "Finding the most potent battery in a bank" $ do
    describe "Given a collection of batteries where the third and fourth battery have the highest voltages" $ do
      it "The third battery and its position is returned" $ do
        let sample = map mkBattery [1, 2, 3, 3]

        let expected = (2, mkBattery 3)
        findMstPotentBattery sample `shouldBe` expected


  describe "Finding the most potent pair of batteries in a bank" $ do
    it "Returns the largest possible combined joltage" $ do
      calcBestJoltage (map mkBattery [9, 8, 7]) `shouldBe` Joltage 98
      calcBestJoltage (map mkBattery [1, 2, 9]) `shouldBe` Joltage 29
      calcBestJoltage (map mkBattery [8, 1, 9]) `shouldBe` Joltage 89

  describe "Finding the sum of the best combined joltages over a large collection of bank" $ do
    it "Returns x" $ do
      (readFile "data/data.txt" & fmap (sumJoltages . map (calcBestJoltage . mkBank) . lines )) `shouldReturn` Joltage 17493

