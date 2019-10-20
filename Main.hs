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
  deriving (Show)

getVolunteers :: Shift -> [Volunteer] -> [Volunteer]
getVolunteers s = filter $ elem s . availability

fillTheGaps :: Shifts -> [Volunteer] -> Schedule
fillTheGaps = undefined -- TODO:

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- Try it out!
turns :: Shifts
turns = ["M1", "M2", "M3"]

vols :: [Volunteer]
vols =
  [ Volunteer "Flavio" ["M1", "M2", "M3"]
  , Volunteer "Lydia" ["M2", "M3"]
  , Volunteer "Eva" ["M3"]
  ]
