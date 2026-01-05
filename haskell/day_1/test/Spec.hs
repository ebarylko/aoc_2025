import Test.Hspec
import DayOne(Dial, DialRotation)

main :: IO ()

main = hspec $ do
  describe "Running a test" $ do
    it "Should not fail" $ do
      pending
