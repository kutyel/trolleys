module Main where

type Shift = String
type Shifts = [Shift] 
type Shedule = [Turn]

data Turn = Empty | Captain Volunteer | Volunteers [Volunteer]

data Volunteer = Volunteer { name :: String, availability :: Shifts }

main :: IO ()
main = putStrLn "Hello, Haskell!"
