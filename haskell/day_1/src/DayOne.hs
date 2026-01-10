module DayOne(
  Dial(..),
  DialRotation(..),
  NonNegative(..),
  rotateDial,
  rotationUnit,
  numOfTimesDialPointsAtZero,
  numOfTimesDialPassesZero,
  numOfTimesDialIsShiftedToAndPastZero
           ) where

import Data.Function ((&))
import Data.Maybe (fromJust)
import Control.Monad (mfilter)
import Data.List (elemIndex)

newtype NonNegative = NonNegative { val:: Int } deriving (Eq, Ord, Show)

extractVal :: NonNegative -> Int

extractVal = val

{-
Takes a number n and returns a value representing n units of
rotation in a direction if n is non-negative. Returns Nothing otherwise.
-}

rotationUnit :: Int -> Maybe NonNegative

maybeFromPredicate :: (a -> Bool) -> a -> Maybe a

maybeFromPredicate predicate = filterMaybe predicate . Just

rotationUnit = fmap NonNegative . maybeFromPredicate (>=0)

type DialRange = NonNegative

{-
This type represents a dial containing the
numbers in [0, 99] in sequential order
-}
newtype Dial = Dial DialRange deriving (Show, Eq)

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


filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a

filterMaybe = mfilter

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

toCanonicalDialValue = modBy 100 . (+) 100 . modBy 100


rotateDial rotation (Dial v) = Dial currentVal
  where currentVal =
          rotation
          & extractRotationShift
          & toCanonicalDialValue . (+ val v)
          & NonNegative


{-
Takes a collection of rotations, a dial, and returns the number of times that
the dial points at zero after applying a rotation
-}
numOfTimesDialPointsAtZero :: [DialRotation] -> Dial -> NonNegative


extractPointedNumber :: Dial -> NonNegative

extractPointedNumber (Dial v) = v

zero = NonNegative 0

dialInZero = Dial zero

numOfTimesDialPointsAtZero rotations d =
  scanl (flip rotateDial) d rotations
  & drop 1
  & filter (== dialInZero)
  & length
  & NonNegative

{-
Takes a rotation, a dial, and returns how many times the dial passed
through zero during the application of the rotation
-}
numOfTimesDialPassesZero :: DialRotation -> Dial -> NonNegative

numOfTimesDialPassesZero rotation dial = NonNegative $ numOfRotationsByHundredUnits + leftShiftConsiderationFactor
  where
    pastDialPos = (val . extractPointedNumber) dial
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
numOfTimesDialIsShiftedToAndPastZero :: [DialRotation] -> Dial -> NonNegative


numOfTimesDialIsShiftedToAndPastZero rotations dial =
  rotations
  & scanl (flip rotateDial) dial
  & zipWith numOfTimesDialPassesZero rotations
  & foldl addNonNegative zero
  where
    addNonNegative (NonNegative a) (NonNegative b) = NonNegative (a + b)
