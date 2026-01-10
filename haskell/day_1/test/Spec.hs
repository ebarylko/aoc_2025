import Test.Hspec
import DayOne(DialRotation(..), rotateDial,Dial(..), NonNegative(..), numOfTimesDialPointsAtZero, numOfTimesDialPassesZero, numOfTimesDialIsShiftedToAndPastZero)
import Data.Function ((&))


dialLeft :: Int -> DialRotation

dialLeft shift
  | shift >= 0 = LeftShift $ NonNegative shift
  | otherwise = error "Shift is negative"


dialRight :: Int -> DialRotation

dialRight shift
  | shift >= 0 = RightShift $ NonNegative shift
  | otherwise = error "Shift is negative"

dial :: Int -> Dial

dial start
  | start `elem` [0 .. 99] = Dial $ NonNegative start
  | otherwise = error "Starting point is outside of valid dial range"

main :: IO ()

main = hspec $ do
  describe "Counting the number of times a dial points at zero after applying a collection of rotations" $ do
    describe "Rotating a dial pointing to zero by one hundred units twice" $ do
      it "Points at zero twice over the period of applying both rotations" $ do
        let rotations = replicate 2 $ dialRight 100
        numOfTimesDialPointsAtZero rotations  (dial 0) `shouldBe` NonNegative 2

    describe "Applying a small series of rotations to a dial pointing to 50" $ do
      it "Points to zero three times in the entire process of applying the rotations" $ do
        let rotations = [dialLeft 68,
                          dialLeft 30,
                          dialRight 48,
                          dialLeft 5,
                          dialRight 60,
                          dialLeft 55,
                          dialLeft 1,
                          dialLeft 99,
                          dialRight 14,
                          dialLeft 82]

        numOfTimesDialPointsAtZero rotations (dial 50) `shouldBe`  NonNegative 3

    describe "Applying a large series of rotations to a dial pointing at 50" $ do
      it "Points to zero 1129 times" $ do
        let rotations = readFile "data/data.txt" & fmap (map read . lines)

        numOfTimesDialPointsAtZero <$> rotations <*> (pure . dial) 50 `shouldReturn` NonNegative 1129

  describe "Rotating a dial" $ do

    describe "Rotating a dial pointing at zero" $ do
      describe "Rotating it by ten units to the right" $ do
        it "Creates a dial which points at ten" $ do
          rotateDial (dialRight 10)  (dial 0) `shouldBe` dial 10

      describe "Rotating it by ten units to the left" $ do
        it "Creates a dial which points at ninety" $ do
          rotateDial (dialLeft 10)  (dial 0) `shouldBe` dial 90

    describe "Rotating a dial pointing at ninety" $ do
      describe "Rotating it by ten units to the right" $ do
        it "Creates a dial which points at zero" $ do
          rotateDial (dialRight 10)  (dial 90) `shouldBe` dial 0

  describe "Counting the number of times zero is pointed to during one or more rotations" $ do

    describe "Applying a small series of rotations to a dial pointing at fifty" $ do
      it "Passes by zero six times in the entire process of applying the rotations" $ do
        let rotations = [dialLeft 68,
                          dialLeft 30,
                          dialRight 48,
                          dialLeft 5,
                          dialRight 60,
                          dialLeft 55,
                          dialLeft 1,
                          dialLeft 99,
                          dialRight 14,
                          dialLeft 82]


        numOfTimesDialIsShiftedToAndPastZero rotations (dial 50) `shouldBe` NonNegative 6

    describe "Applying a large series of rotations to a dial pointing at fifty" $ do
      it "Is shifted to zero 6638 times" $ do
        let rotations = readFile "data/data.txt" & fmap (map read . lines)

        numOfTimesDialIsShiftedToAndPastZero <$> rotations <*> (pure . dial) 50 `shouldReturn` NonNegative 6638

    describe "Applying a rotation to a dial pointing at 1" $ do
      describe "Applying a rotation of one hundred and one units to the left" $ do
        it "The dial passes by zero two times" $ do
          numOfTimesDialPassesZero (dialLeft 101)  (dial 1) `shouldBe` NonNegative 2

      describe "Applying a rotation of three units to the right" $ do
        it "The dial never passes by zero " $ do
          numOfTimesDialPassesZero (dialRight 3)  (dial 1) `shouldBe`  NonNegative 0

    describe "Applying a rotation to a dial pointing at zero" $ do
      describe "Rotating the dial by five units to the left" $ do
        it "Does not pass by zero" $ do
          numOfTimesDialPassesZero (dialLeft 5)  (dial 0) `shouldBe` NonNegative 0


  describe "Reading a string representation of a rotation" $ do
    describe "Reading a representation of a right shift of thirty units" $ do
      it "Returns a right shift of thirty units" $ do
        read "R30" `shouldBe` dialRight 30
    describe "Reading a representation of a left shift of thirty units" $ do
      it "Returns a left shift of thirty units" $ do
        read "L30" `shouldBe` dialLeft 30






