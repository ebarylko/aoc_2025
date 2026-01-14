import Test.Hspec
import DayTwo(NonNegative(..), ProductIdVerfificationResult, verifyId, ValidId(..), InvalidId(..))

main :: IO ()
main = hspec $ do
  describe "Categorizing ids" $ do
    describe "Identifying a valid id" $ do
      describe "Given a number with an odd number of digits" $ do
        it "Identifies it as a valid id" $ do
          verifyId (NonNegative 338) `shouldBe` Right ValidId
      describe "Given a number with an even number of digits that does not consist of a repetition of another number" $ do
        it "Identifies it as a valid id" $ do
          verifyId (NonNegative 3334) `shouldBe` Right ValidId

    describe "Identifying an invalid id" $ do
      describe "Given a number that is a repetition of another number" $ do
        it "Identifies it as an invalid id" $ do
          verifyId (NonNegative 1010) `shouldBe` (Left . InvalidId) (NonNegative 1010)
          verifyId (NonNegative 6464) `shouldBe` (Left . InvalidId) (NonNegative 6464)
