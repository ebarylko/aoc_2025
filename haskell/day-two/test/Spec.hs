import Test.Hspec
import DayTwo(NonNegative(..), CategorizedId(..), categorizeId)

main :: IO ()
main = hspec $ do
  describe "Categorizing ids" $ do
    describe "Identifying a valid id" $ do
      describe "Given a number with an odd number of digits" $ do
        it "Identifies it as a valid id" $ do
          categorizeId (NonNegative 3) `shouldBe` ValidId 
