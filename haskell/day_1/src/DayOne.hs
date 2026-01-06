module DayOne(
  Dial(..),
  DialRotation(..),
  mkDial,
  rotateDial
           ) where

import Data.Function ((&))

{-
This type represents a dial containing the
numbers in [0, 99] in sequential order
-}
newtype Dial = CurrentPointedNumber Int deriving (Show, Eq)

{-
This type represents how much a dial is to be rotated by and
in which direction 
-}
data DialRotation = LeftShift Int | RightShift Int

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a

filterMaybe pred m = m >>= (\el -> if pred el then Just el else Nothing)

mkDial :: Int -> Maybe Dial

mkDial = fmap CurrentPointedNumber . filterMaybe (100 >=) . Just

{-
Takes a rotation to apply to a dial, a dial, and returns
the result of applying the rotation to the dial
-}
rotateDial :: DialRotation -> Dial -> Dial

extractRotationShift :: DialRotation -> Int

extractRotationShift (LeftShift v) = -v
extractRotationShift (RightShift v) = v

{-
Takes a modulus, m, a value x, and returns the
result of applying x mod m
-}
modBy :: Int -> Int -> Int

modBy m v = v `mod` m

{-
Takes a value pointed to by the dial and canonicalizes the value so it lies in
in [0, 99]
-}
toCanonicalDialValue :: Int -> Int

toCanonicalDialValue = modBy 100 . (modBy 100 . (+) 100)



rotateDial rotation (CurrentPointedNumber v) = CurrentPointedNumber currentVal where currentVal = rotation & extractRotationShift & (toCanonicalDialValue . (+ v))
