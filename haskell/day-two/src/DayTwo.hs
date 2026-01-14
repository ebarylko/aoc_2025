module DayTwo
    (NonNegative(..), ProductIdVerfificationResult, verifyId, ValidId(..), InvalidId(..), UnverifiedProductId, filterInvalidIds, calcInvalidIdsSum
    ) where

import Data.Char (digitToInt)
import Data.Either (lefts)

newtype NonNegative = NonNegative { val:: Int } deriving (Eq, Show)

{-
This type represents a product ID where it is unclear if it is
valid or invalid
-}
type UnverifiedProductId = NonNegative


data ValidId = ValidId deriving  (Eq, Show)

newtype InvalidId = InvalidId UnverifiedProductId deriving (Eq, Show)

eitherFromPred :: (a -> Bool) -> (a -> b) -> (a -> c) -> a -> Either b c

eitherFromPred predicate leftFn rightFn x =
  if predicate x
  then (Right . rightFn) x
  else (Left . leftFn) x

{-
This data type represents the two different types of product ids in the gift shop
database, being valid if the id does not consist of a repetition of digits.
It is invalid otherwise.
-}
type ProductIdVerfificationResult = Either InvalidId ValidId

{-
Takes a product id that may be valid or invalid, and labels it
as invalid or valid according to the rules above otherwise.
-}
verifyId :: UnverifiedProductId -> ProductIdVerfificationResult

verifyId  = eitherFromPred isValidId InvalidId (const ValidId)
  where isValidId = liftA2 (||) hasOddNumberOfDigits isNotRepeatedSequence
        hasOddNumberOfDigits = (== 1) . flip mod 2 . length . extractDigits

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

type IdRange = [UnverifiedProductId]

{-
Given a collection of unverified ids, returns all the invalid
ids
-}
filterInvalidIds :: IdRange -> [InvalidId]

filterInvalidIds = lefts . map verifyId

{--
Given a collection of product id ranges, filters
out the invalid ids in each range and returns their
sum.
-}
calcInvalidIdsSum  :: [IdRange] -> NonNegative

--calcInvalidIdsSum ids = 


calcInvalidIdsSum _ = NonNegative 0


