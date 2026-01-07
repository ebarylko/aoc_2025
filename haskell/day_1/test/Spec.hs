import Test.Hspec
import DayOne(DialRotation(..), mkDial, rotateDial, mkShift, numOfTimesDialPointsAtZero)
import Data.Maybe (catMaybes)


main :: IO ()

main = hspec $ do
  describe "Creating a dial that does not point to a number in [0, 99]" $ do
    it "Returns Nothing" $ do
      mkDial (-1) `shouldBe` Nothing

  describe "Rotating a dial pointing at zero" $ do
    describe "Rotating it by ten units to the right" $ do
      it "Creates a dial which points at ten" $ do
        rotateDial <$> mkShift RightShift 10 <*>  mkDial 0 `shouldBe` mkDial 10

    describe "Rotating it by ten units to the left" $ do
      it "Creates a dial which points at ninety" $ do
        rotateDial <$> mkShift LeftShift 10 <*>  mkDial 0 `shouldBe` mkDial 90

  describe "Rotating a dial pointing at ninety" $ do
    describe "Rotating it by ten units to the right" $ do
      it "Creates a dial which points at zero" $ do
        rotateDial <$> mkShift RightShift 10 <*>  mkDial 90 `shouldBe` mkDial 0

  describe "Rotating a dial pointing to zero by one hundred units twice" $ do
    it "Points at zero twice over the period of applying both rotations" $ do
      let rotations = catMaybes $ replicate 2 $ mkShift RightShift 100
      numOfTimesDialPointsAtZero rotations <$> mkDial 0 `shouldBe` Just 2

  describe "Reading a string representation of a rotation" $ do
    describe "Reading a representation of a right shift of thirty units" $ do
      it "Returns a right shift of thirty units" $ do
        (Just . read) "R30" `shouldBe` mkShift RightShift 30
    describe "Reading a representation of a left shift of thirty units" $ do
      it "Returns a left shift of thirty units" $ do
        (Just . read) "L30" `shouldBe` mkShift LeftShift 30


