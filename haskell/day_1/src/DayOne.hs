module DayOne(
  Dial(..),
  DialRotation(..),
  mkDial,
  rotateDial
           ) where


{-
This type represents a dial containing the
numbers in [0, 99] in sequential order
-}
newtype Dial = CurrentPointedNumber Int deriving (Show, Eq)

{-
This type represents how much a dial is to be rotated by and
in which direction 
-}
data DialRotation = Left Int | Right Int

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a

filterMaybe pred m = m >>= (\el -> if pred el then Just el else Nothing)

mkDial :: Int -> Maybe Dial

mkDial = fmap CurrentPointedNumber . filterMaybe (100 >=) . Just

rotateDial :: DialRotation -> Dial -> Dial

rotateDial _ d = d
