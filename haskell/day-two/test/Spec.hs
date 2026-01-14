import Test.Hspec
import DayTwo(UnverifiedProductId, ProductIdVerfificationResult, NonNegative(..), verifyId, ValidId(..), InvalidId(..), filterInvalidIds)

{--
Takes a number and generates an id that may be valid/invalid
-}
unverifiedId :: Int -> UnverifiedProductId

unverifiedId = NonNegative

mkInvalidId :: UnverifiedProductId -> ProductIdVerfificationResult

mkInvalidId = Left . InvalidId

main :: IO ()
main = hspec $ do
  describe "Categorizing ids" $ do
    describe "Identifying a valid id" $ do
      describe "Given a number with an odd number of digits" $ do
        it "Identifies it as a valid id" $ do
          verifyId (unverifiedId 338) `shouldBe` Right ValidId
      describe "Given a number with an even number of digits that does not consist of a repetition of another number" $ do
        it "Identifies it as a valid id" $ do
          verifyId (unverifiedId 3334) `shouldBe` Right ValidId

    describe "Identifying an invalid id" $ do
      describe "Given a number that is a repetition of another number" $ do
        it "Identifies it as an invalid id" $ do
          verifyId (unverifiedId 1010) `shouldBe` mkInvalidId (unverifiedId 1010)
          verifyId (unverifiedId 6464) `shouldBe` mkInvalidId (unverifiedId 6464)

    describe "Identifying all invalid ids in a collection of ids" $ do
      describe "Given the ids in the range [11, 22]" $ do
        it "Only 11 and 22 are found to be invalid" $ do
          let expected = [mkInvalidId (unverifiedId 11), mkInvalidId (unverifiedId 22)]

          filterInvalidIds (map unverifiedId [11 .. 22]) `shouldBe` expected
