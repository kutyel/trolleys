{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Data.Text             (Text (..), unpack)
import qualified Data.Yaml             as Y
import           GHC.Generics
import           System.Random
import           System.Random.Shuffle (shuffleM)

type Shift = Text

type Shifts = [Shift]

type Schedule = [Turn]

data Config =
  Config
    { shifts     :: Shifts
    , volunteers :: [Text]
    }
  deriving (Generic)

instance FromJSON Config

data Turn
  = Empty
  | Captain Volunteer
  | Volunteers [Volunteer]
  deriving (Show)

data Volunteer =
  Volunteer
    { name         :: Text
    , availability :: Shifts
    }
  deriving (Generic)

instance FromJSON Volunteer

instance Show Volunteer where
  show = unpack . name

-- helpers
getVolunteers :: [Volunteer] -> Shift -> [Volunteer]
getVolunteers = flip $ filter . (. availability) . elem

fillSchedule :: Int -> [Volunteer] -> Turn
fillSchedule n []  = Empty
fillSchedule n [p] = Captain p
fillSchedule n xs  = Volunteers $ take n xs

-- the first `Int` is the # of volunteers per shift!
fillTheGaps :: Int -> [Volunteer] -> Shifts -> IO Schedule
fillTheGaps n vols =
  mapM $ (pure . fillSchedule n =<<) . shuffleM . getVolunteers vols

main :: IO Schedule
main = do
  content <- BS.readFile "config.yml"
  let parsed = Y.decodeThrow content :: Maybe Config
  case parsed of
    Nothing              -> error "Could not parse config file!"
    (Just (Config ss _)) -> fillTheGaps 2 vols ss

-- Try it out!
vols :: [Volunteer]
vols =
  [ Volunteer "Flavio" ["M", "T", "W"]
  , Volunteer "Lydia" ["T", "W"]
  , Volunteer "Eva" ["W"]
  ]
