module DayThree
    (findMstPotentBattery,
     Battery(..),
     Joltage(..)
    ) where

import Control.Arrow ((>>>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Ord (Down(..))

-- Represents a value in [1, 9]
newtype Joltage = Joltage  Int deriving (Eq, Show, Ord)

newtype Battery = Battery Joltage deriving (Eq, Show, Ord)

-- Represents a bank of batteries where at least two are present
type Bank = [Battery]




 -- Takes a collection of batteries and returns the
 -- position and the battery of the one with the highest joltage.

findMstPotentBattery :: Bank -> (Int, Battery)

getJoltage :: Battery -> Joltage

getJoltage (Battery v) = v

findMstPotentBattery = enumerate >>> sortBy (compare `on` joltageDecreasing) >>> head
  where
    joltage = getJoltage . snd
    joltageDecreasing = Down .joltage
    enumerate = zip [0 ..]



