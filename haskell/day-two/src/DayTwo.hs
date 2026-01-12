module DayTwo
    (NonNegative(..), ProductId(..), categorizeId
    ) where

import Data.Char (digitToInt)
import Data.Function ((&))

newtype NonNegative = NonNegative { val:: Int } deriving (Eq, Show)

{-
This type represents a product ID where it is unclear if it is
valid or invalid
-}
type UnverifiedProductId = NonNegative

{-
This data type represents the two different types of product ids in the gift shop
database, being valid if the id does not consist of a repetition of digits.
It is invalid otherwise.
-}
data ProductId = ValidId | InValidId NonNegative deriving (Eq, Show)

{-
Takes a product id that may be valid or invalid, and labels it
as invalid or valid according to the rules above otherwise.
-}
categorizeId :: UnverifiedProductId -> ProductId

categorizeId a = a
  & maybeFromPred isValidId
  & maybe (InValidId a) (const ValidId)
  where isValidId = liftA2 (||) hasEvenNumberOfDigits isNotRepeatedSequence
        hasEvenNumberOfDigits = (== 0) . flip mod 2 . length . extractDigits

maybeFromPred :: (a -> Bool) -> a -> Maybe a

maybeFromPred predicate a = if predicate a then Just a else Nothing

{-
Takes a non-negative number which is assumed to be even and
returns true if the number does not consist of a repeated
number. For example, with 54 and 1013 as input, the function
returns false. With 5454 and 1010, the function returns false.
-}
isNotRepeatedSequence :: NonNegative -> Bool

isNotRepeatedSequence num = firstHalf /= secondHalf
  where
    digits = extractDigits num
    halfwayPoint = length digits `div` 2
    ( firstHalf, secondHalf ) = splitAt halfwayPoint digits



{-
Takes a non-negative number and returns a collection of the
digits of the number
-}
extractDigits :: NonNegative -> [NonNegative]

extractDigits num = map (NonNegative . digitToInt) $ (show . val) num
