#!/usr/bin/env stack
-- stack --resolver lts-12.20 script --package process --package old-time

import Control.Exception (bracket)
import System.Environment (getArgs)
import System.Process (callCommand)
import System.Time (ClockTime, TimeDiff(..), getClockTime, diffClockTimes)
import Text.Printf

exename :: String
exename = "advent-exe"

main :: IO()
main = getArgs >>= build

build :: [String] -> IO()
build ("build":_) = callCommand "stack build"
build ("test":_)  = callCommand "stack test"
build _           = callCommand "stack build" >> timeExecution (callCommand $ "stack exec " ++ exename)

timeExecution :: IO() -> IO()
timeExecution f = 
  bracket
    getClockTime
    printTimeDiff
    (\_ -> f)

printTimeDiff :: ClockTime -> IO()
printTimeDiff st = do
  et <- getClockTime
  let (TimeDiff _ _ _ h m s p) = diffClockTimes et st
  let secs = fromIntegral s + (fromIntegral p / 1000000000000) :: Float
  putStrLn $ printf "Time Taken: %d:%d:%f" h m secs