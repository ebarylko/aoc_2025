import Test.Hspec
import DayOne(Dial, DialRotation(..), mkDial, rotateDial)

main :: IO ()

main = hspec $ do
  describe "Rotating a dial pointing at zero" $ do
    describe "Rotating it by ten units" $ do
      it "Creates a dial which points at ten" $ do
        rotateDial (DayOne.Right 10) <$> mkDial 0 `shouldBe` mkDial 10

