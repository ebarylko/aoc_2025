import Test.Hspec
import DayTwo(NonNegative(..), ProductId(..), categorizeId, ValidId(..), InValidId(..))

main :: IO ()
main = hspec $ do
  describe "Categorizing ids" $ do
    describe "Identifying a valid id" $ do
      describe "Given a number with an odd number of digits" $ do
        it "Identifies it as a valid id" $ do
          categorizeId (NonNegative 338) `shouldBe` Right ValidId
      describe "Given a number with an even number of digits that does not consist of a repetition of another number" $ do
        it "Identifies it as a valid id" $ do
          categorizeId (NonNegative 3334) `shouldBe` Right ValidId
