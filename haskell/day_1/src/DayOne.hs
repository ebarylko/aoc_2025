module DayOne(
  Dial(..),
  DialRotation(..),
  mkDial,
  rotateDial,
  rotationUnit,
  mkShift,
  numOfTimesDialPointsAtZero,
  numOfTimesDialPassesZero
           ) where

import Data.Function ((&))
import Data.Maybe (fromJust)

newtype NonNegative = NonNegative { val:: Int } deriving (Eq, Ord, Show)

extractVal :: NonNegative -> Int

extractVal = val

{-
Takes a number n and returns a value representing n units of
rotation in a direction if n is non-negative. Returns Nothing otherwise.
-}

rotationUnit :: Int -> Maybe NonNegative

rotationUnit num = Just num & filterMaybe isNonNegative & fmap NonNegative where isNonNegative = (0 <=)

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

filterMaybe p m = m >>= (\el -> if p el then Just el else Nothing)

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

inc :: Num a => a -> a

inc = (+ 1)

numOfTimesDialPointsAtZero rotations d = foldl' updateZeroPointerCount (d, 0) rotations & snd
  where updateZeroPointerCount (currDial, currCount) rotation  =
          let newDial = rotateDial rotation currDial in
            (newDial, if pointsToZero newDial then inc currCount else currCount)

        extractPointedNumber (CurrentPointedNumber v) = v
        pointsToZero = (== 0) . extractPointedNumber

{-
Takes a rotation, a dial, and returns how many times the dial passed
through zero during the application of the rotation
-}
numOfTimesDialPassesZero :: DialRotation -> Dial -> Int

numOfTimesDialPassesZero _ _ = 0


instance Read DialRotation where
  readsPrec _ (direction : shift) = [(toDirection direction (read shift & rotationUnit & fromJust), "")]
    where toDirection d = case d of
            'L' -> LeftShift
            _ -> RightShift
  readsPrec _ _ = []

