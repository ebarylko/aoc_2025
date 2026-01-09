import Test.Hspec
import DayOne(DialRotation(..), mkDial, rotateDial, mkShift, numOfTimesDialPointsAtZero, numOfTimesDialPassesZero, numOfTimesDialIsShiftedToAndPastZero)
import Data.Maybe (catMaybes, fromJust)
import Data.Function ((&))


main :: IO ()

main = hspec $ do
  describe "Counting the number of times a dial points at zero after applying a collection of rotations" $ do
    describe "Rotating a dial pointing to zero by one hundred units twice" $ do
      it "Points at zero twice over the period of applying both rotations" $ do
        let rotations = catMaybes $ replicate 2 $ mkShift RightShift 100
        numOfTimesDialPointsAtZero rotations <$> mkDial 0 `shouldBe` Just 2

    describe "Applying a small series of rotations to a dial pointing to 50" $ do
      it "Points to zero three times in the entire process of applying the rotations" $ do
        let rotations = catMaybes [mkShift LeftShift 68,
                                   mkShift LeftShift 30,
                                   mkShift RightShift 48,
                                   mkShift LeftShift 5,
                                   mkShift RightShift 60,
                                   mkShift LeftShift 55,
                                   mkShift LeftShift 1,
                                   mkShift LeftShift 99,
                                   mkShift RightShift 14,
                                   mkShift LeftShift 82]

        numOfTimesDialPointsAtZero rotations <$> mkDial 50 `shouldBe` Just 3

    describe "Applying a large series of rotations to a dial pointing at 50" $ do
      it "Points to zero 1129 times" $ do
        let rotations = readFile "data/data.txt" & fmap (map read . lines)

        numOfTimesDialPointsAtZero <$> rotations <*> (pure . fromJust . mkDial) 50 `shouldReturn` 1129


  describe "Creating a dial that does not point to a number in [0, 99]" $ do
    it "Returns Nothing" $ do
      mkDial (-1) `shouldBe` Nothing

  describe "Rotating a dial" $ do

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



  describe "Counting the number of times zero is pointed to during one or more rotations" $ do

    describe "Applying a small series of rotations to a dial pointing at fifty" $ do
      it "Passes by zero six times in the entire process of applying the rotations" $ do
        let rotations = catMaybes [mkShift LeftShift 68,
                                   mkShift LeftShift 30,
                                   mkShift RightShift 48,
                                   mkShift LeftShift 5,
                                   mkShift RightShift 60,
                                   mkShift LeftShift 55,
                                   mkShift LeftShift 1,
                                   mkShift LeftShift 99,
                                   mkShift RightShift 14,
                                   mkShift LeftShift 82]


        numOfTimesDialIsShiftedToAndPastZero rotations <$> mkDial 50 `shouldBe` Just 6

    describe "Applying a large series of rotations to a dial pointing at fifty" $ do
      it "Is shifted to zero 6638 times" $ do
        let rotations = readFile "data/data.txt" & fmap (map read . lines)

        numOfTimesDialIsShiftedToAndPastZero <$> rotations <*> (pure . fromJust . mkDial) 50 `shouldReturn` 6638

    describe "Applying a rotation to a dial pointing at 1" $ do
      describe "Applying a rotation of one hundred and one units to the left" $ do
        it "The dial passes by zero two times" $ do
          numOfTimesDialPassesZero <$> mkShift LeftShift 101 <*> mkDial 1 `shouldBe` Just 2

      describe "Applying a rotation of three units to the right" $ do
        it "The dial never passes by zero " $ do
          numOfTimesDialPassesZero <$> mkShift RightShift 3 <*> mkDial 1 `shouldBe` Just 0

    describe "Applying a rotation to a dial pointing at zero" $ do
      describe "Rotating the dial by five units to the left" $ do
        it "Does not pass by zero" $ do
          numOfTimesDialPassesZero <$> mkShift LeftShift 5 <*> mkDial 0 `shouldBe` Just 0


  describe "Reading a string representation of a rotation" $ do
    describe "Reading a representation of a right shift of thirty units" $ do
      it "Returns a right shift of thirty units" $ do
        (Just . read) "R30" `shouldBe` mkShift RightShift 30
    describe "Reading a representation of a left shift of thirty units" $ do
      it "Returns a left shift of thirty units" $ do
        (Just . read) "L30" `shouldBe` mkShift LeftShift 30






