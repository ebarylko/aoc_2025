import Test.Hspec
import DayOne(DialRotation(..), mkDial, rotateDial, mkShift)

main :: IO ()

main = hspec $ do
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

