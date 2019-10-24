{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.List             (intercalate)
import           Data.Text             (Text (..), unpack)
import qualified Data.Yaml             as Y
import           GHC.Generics
import           System.IO
import           System.Random.Shuffle (shuffleM)

type Shift = Text

type Shifts = [Shift]

type Schedule = [Turn]

data Config =
  Config
    { shifts     :: Shifts
    , volunteers :: [Volunteer]
    }
  deriving (Generic)

instance FromJSON Config

data Turn
  = Empty
  | Captain Volunteer
  | Volunteers [Volunteer]

instance Show Turn where
  show Empty           = "EMPTY!\n"
  show (Captain v)     = "ONLY " ++ show v ++ ".\n"
  show (Volunteers vs) = intercalate ", " (map show vs) ++ ".\n"

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
fillTheGaps n =
  traverse . (((pure . fillSchedule n =<<) . shuffleM) .) . getVolunteers

main :: IO Schedule
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "What is your config YAML file?"
  config <- getLine
  content <- BS.readFile config
  let parsed = Y.decodeThrow content
  case parsed of
    Nothing             -> error "Could not parse config file!"
    Just (Config ss vs) -> fillTheGaps 2 vs ss
