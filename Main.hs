{-# language DeriveAnyClass #-}

module Main where

import Control.Monad.Random (MonadRandom (getRandomR))
import Data.Aeson (FromJSON)
import Data.ByteString.Char8 qualified as BS
import Data.List (intercalate)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import GHC.Generics (Generic)
import System.IO

type Shift = T.Text
type Shifts = [Shift]
type Schedule = [Turn]

data Config = Config
  { shifts :: Shifts
  , volunteers :: [Volunteer]
  }
  deriving (Generic, FromJSON)

data Turn
  = Empty
  | Captain Volunteer
  | Volunteers [Volunteer]

instance Show Turn where
  show :: Turn -> String
  show Empty = "EMPTY!\n"
  show (Captain v) = "ONLY " ++ show v ++ ".\n"
  show (Volunteers vs) = intercalate ", " (map show vs) ++ ".\n"

data Volunteer = Volunteer
  { name :: T.Text
  , availability :: Shifts
  , pioneer :: Bool
  , spouse :: Maybe T.Text
  }
  deriving (Eq, FromJSON, Generic)

instance Show Volunteer where
  show :: Volunteer -> String
  show = T.unpack . name

-- helpers
getVolunteers :: [Volunteer] -> Shift -> [Volunteer]
getVolunteers = flip $ filter . (. availability) . elem

removeFromList :: (Eq a) => a -> [a] -> [a]
removeFromList deleted xs = [x | x <- xs, x /= deleted]

fillSchedule :: Int -> [Volunteer] -> [Volunteer] -> IO Turn
fillSchedule 0 currentTurn _ = pure $ Volunteers currentTurn
fillSchedule n currentTurn vols = do
  pickedVolunteer <- extractR vols
  let newVolunteers = removeFromList pickedVolunteer vols
  fillSchedule (n - 1) (pickedVolunteer : currentTurn) newVolunteers

extractR :: (MonadRandom m) => [a] -> m a
extractR xs = (xs !!) <$> getRandomR (0, length xs - 1)

fillTheGaps :: Int -> [Volunteer] -> Shifts -> IO Schedule
fillTheGaps gapsPerShift volunteers =
  traverse
    ( \shift -> do
        let availableVolunteers = getVolunteers volunteers shift
        fillSchedule gapsPerShift [] availableVolunteers
    )

main :: IO Schedule
main = do
  hSetBuffering stdout NoBuffering
  content <- BS.readFile "config.yml"
  let parsed = Y.decodeThrow content
  case parsed of
    Nothing -> error "Could not parse config file!"
    Just (Config ss vs) -> fillTheGaps 3 vs ss -- TODO: pass this as argv
