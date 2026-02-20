module DayThree
    (mkBattery,
     findMstPotentBattery
    ) where

-- Represents a value in [1, 9]
newtype Joltage = Joltage  Int deriving (Eq, Show, Ord)

newtype Battery = Battery Joltage deriving (Eq, Show, Ord)

-- Represents a bank of batteries where at least two are present
type Bank = [Battery]


-- Takes a number n in [1, 9] and creates a battery with a joltage of n
mkBattery :: Int -> Battery

mkBattery = Battery . Joltage


findMstPotentBattery :: Bank -> (Int, Battery)

findMstPotentBattery _ = (0, mkBattery 0)
