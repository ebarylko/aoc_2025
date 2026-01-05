module DayOne(
  Dial(..),
  DialRotation(..)
           ) where


newtype Dial = CurrentPointedNumber Int

data DialRotation = Left Int | Right Int
