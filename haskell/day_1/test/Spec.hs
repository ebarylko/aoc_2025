import Test.Hspec
import DayOne(Dial, DialRotation(..), mkDial, rotateDial)

main :: IO ()

main = hspec $ do
  describe "Rotating a dial pointing at zero" $ do
    describe "Rotating it by ten units to the right" $ do
      it "Creates a dial which points at ten" $ do
        rotateDial (RightShift 10) <$> mkDial 0 `shouldBe` mkDial 10
    describe "Rotating it by ten units to the left" $ do
      it "Creates a dial which points at ninety" $ do
        rotateDial (LeftShift 10) <$> mkDial 0 `shouldBe` mkDial 90

