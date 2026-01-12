module DayTwo
    (NonNegative(..), CategorizedId(..), categorizeId
    ) where
import qualified Control.Applicative as digits



newtype NonNegative = NonNegative { val:: Int } deriving (Eq, Show)

type UncategorizedId = NonNegative

{-
This data type represents the two different types of ids in the gift shop
database, being valid if the id does not consist of a repetition of digits.
It is invalid otherwise.
-}
data CategorizedId = ValidId | InValidId NonNegative deriving (Eq, Show)

{-
Takes an id that may be valid or invalid, and labels it
as invalid or valid according to the rules above otherwise.
-}
categorizeId :: UncategorizedId -> CategorizedId

categorizeId _ = InValidId $ NonNegative 0
