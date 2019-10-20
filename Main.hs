module Main where

import           Data.Maybe
import           System.Random
import           System.Random.Shuffle (shuffleM)

type Shift = String

type Shifts = [Shift]

type Schedule = [Turn]

data Turn
  = Empty
  | Captain Volunteer
  | Volunteers [Volunteer]

data Volunteer =
  Volunteer
    { name         :: String
    , availability :: Shifts
    }

fillTheGaps :: Shifts -> [Volunteer] -> Schedule
fillTheGaps = undefined -- TODO:

main :: IO ()
main = putStrLn "Hello, Haskell!"
