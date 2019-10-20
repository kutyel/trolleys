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
  deriving (Show)

data Volunteer =
  Volunteer
    { name         :: String
    , availability :: Shifts
    }
  deriving (Show)

-- helpers
getVolunteers :: Shift -> [Volunteer] -> [Volunteer]
getVolunteers s = filter $ elem s . availability

fillSchedule :: Int -> [Volunteer] -> Turn
fillSchedule n []  = Empty
fillSchedule n [p] = Captain p
fillSchedule n xs  = Volunteers $ take n xs

-- the first `Int` is the # of volunteers per shift!
fillTheGaps :: Int -> [Volunteer] -> Shifts -> IO Schedule
fillTheGaps n vols =
  mapM
    (\s -> do
       vs <- shuffleM $ getVolunteers s vols
       pure $ fillSchedule n vs)

main :: IO Schedule
main = fillTheGaps 2 vols turns

-- Try it out!
turns :: Shifts
turns = ["M1", "M2", "M3", "W1", "W2"]

vols :: [Volunteer]
vols =
  [ Volunteer "Flavio" ["M1", "M2", "M3"]
  , Volunteer "Lydia" ["M2", "M3"]
  , Volunteer "Eva" ["M3"]
  ]
