import Test.Hspec
import DayTwo(UnverifiedProductId, ProductIdVerfificationResult, NonNegative(..), verifyId, ValidId(..), InvalidId(..), filterInvalidIds, calcInvalidIdsSum)

{--
Takes a number and generates an id that may be valid/invalid
-}
unverifiedId :: Int -> UnverifiedProductId

unverifiedId = NonNegative

mkInvalidId :: UnverifiedProductId -> ProductIdVerfificationResult

mkInvalidId = Left . InvalidId

mkIdRange :: [Int] -> [UnverifiedProductId]

mkIdRange = map unverifiedId


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
          let expected = map (InvalidId . unverifiedId) [11, 22]

          filterInvalidIds (map unverifiedId [11 .. 22]) `shouldBe` expected

      describe "Given the ids in the range [565653,565659]" $ do
        it "No ids are found to be invalid" $ do
          filterInvalidIds (map unverifiedId [565653 .. 565659]) `shouldBe` []
  describe "Summing all invalid ids over a collection of id ranges" $ do
    it "Returns 1227775554 " $ do
      let ranges = map mkIdRange [[11 .. 22],
                                  [95 .. 115],
                                  [998 .. 1012],
                                  [1188511880 .. 1188511890],
                                  [222220 .. 222224],
                                  [1698522 .. 1698528],
                                  [446443 .. 446449],
                                  [38593856 .. 38593862]]

      calcInvalidIdsSum ranges `shouldBe` NonNegative 1227775554
