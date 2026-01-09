module DayOne(
  Dial(..),
  DialRotation(..),
  mkDial,
  rotateDial,
  rotationUnit,
  mkShift,
  numOfTimesDialPointsAtZero,
  numOfTimesDialPassesZero,
  numOfTimesDialIsShiftedToAndPastZero
           ) where

import Data.Function ((&))
import Data.Maybe (fromJust)
import Control.Monad (mfilter)

newtype NonNegative = NonNegative { val:: Int } deriving (Eq, Ord, Show)

extractVal :: NonNegative -> Int

extractVal = val

{-
Takes a number n and returns a value representing n units of
rotation in a direction if n is non-negative. Returns Nothing otherwise.
-}

rotationUnit :: Int -> Maybe NonNegative

rotationUnit num = NonNegative <$> filterMaybe (>=0) (Just num)

{-
This type represents a dial containing the
numbers in [0, 99] in sequential order
-}
newtype Dial = CurrentPointedNumber Int deriving (Show, Eq)

{-
This type represents how much a dial is to be rotated by and
in which direction
-}
data DialRotation = LeftShift NonNegative | RightShift NonNegative deriving (Show, Eq)

{-
Takes a direction to shift the dial in (left, right), an amount to shift by
that may be valid, and returns a representation of shifting in the given
direction by the passed amount if the amount is non-negative. Returns
Nothing otherwise.
-}

mkShift :: (NonNegative -> DialRotation) -> Int -> Maybe DialRotation

mkShift direction shift = direction <$> rotationUnit shift

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a

filterMaybe = mfilter 

mkDial :: Int -> Maybe Dial

mkDial = fmap CurrentPointedNumber . filterMaybe (`elem` [0 .. 99]) . Just

{-
Takes a rotation to apply to a dial, a dial, and returns
the result of applying the rotation to the dial
-}
rotateDial :: DialRotation -> Dial -> Dial

extractRotationShift :: DialRotation -> Int

extractRotationShift (LeftShift v) = (negate . extractVal) v
extractRotationShift (RightShift v) = extractVal v

{-
Takes a modulus, m, a value x, and returns the
result of applying x mod m
-}
modBy :: Int -> Int -> Int

modBy = flip mod

{-
Takes a value pointed to by the dial and canonicalizes the value so it lies in
in [0, 99]
-}
toCanonicalDialValue :: Int -> Int

toCanonicalDialValue = modBy 100 . (modBy 100 . (+) 100)



rotateDial rotation (CurrentPointedNumber v) = CurrentPointedNumber currentVal where currentVal = rotation & extractRotationShift & (toCanonicalDialValue . (+ v))

{-
Takes a collection of rotations, a dial, and returns the number of times that
the dial points at zero after applying a rotation
-}
numOfTimesDialPointsAtZero :: [DialRotation] -> Dial -> Int


extractPointedNumber :: Dial -> Int

extractPointedNumber (CurrentPointedNumber v) = v

numOfTimesDialPointsAtZero rotations d = scanl (flip rotateDial) d rotations & drop 1 & filter ((== 0) . extractPointedNumber) & length

{-
Takes a rotation, a dial, and returns how many times the dial passed
through zero during the application of the rotation
-}
numOfTimesDialPassesZero :: DialRotation -> Dial -> Int

-- {-
-- Takes a predicate, a number, and increments the number if it
-- satisfies the predicate. Returns the same number otherwise
-- -}
-- incIf :: Num a => (a -> bool) -> a -> a

numOfTimesDialPassesZero rotation dial = numOfRotationsByHundredUnits + leftShiftConsiderationFactor
  where
    pastDialPos = extractPointedNumber dial
    currDialPos = extractRotationShift rotation + pastDialPos
    numOfRotationsByHundredUnits = abs (currDialPos `quot` 100)
    isNotPointingAtZero = (0 /=)
    leftShiftConsiderationFactor = if currDialPos <= 0 && isNotPointingAtZero pastDialPos then 1 else 0


{-
Assume that the input passed to read has the form Ld or Rd, where d is an integer
-}
instance Read DialRotation where
  readsPrec _ (direction : shift) = [(toDirection direction (read shift & rotationUnit & fromJust), "")]
    where toDirection d = case d of
            'L' -> LeftShift
            _ -> RightShift
  readsPrec _ _ = []


{-
Takes a collection of rotations, a dial, and returns the number of times the dial
is shifted such that it lands at zero or passes through it when applying the rotations
-}
numOfTimesDialIsShiftedToAndPastZero :: [DialRotation] -> Dial -> Int

-- numOfTimesDialIsShiftedToAndPastZero rotations dial = foldl' updateZeroShiftCount (dial, 0) rotations & snd
--   where updateZeroShiftCount (currDial, currCount) rotation = (rotateDial rotation currDial, numOfTimesDialPassesZero rotation currDial + currCount)

numOfTimesDialIsShiftedToAndPastZero rotations dial = rotations & scanl (flip rotateDial) dial & zipWith numOfTimesDialPassesZero rotations & sum
