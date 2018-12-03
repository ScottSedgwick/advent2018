#!/usr/bin/env stack
-- stack --resolver lts-12.20 script --package process --package old-time

import System.Environment (getArgs)
import System.Process (callCommand)
import System.Time (TimeDiff(..), getClockTime, diffClockTimes)

main :: IO()
main = getArgs >>= parse

parse :: [String] -> IO()
parse ("build":_) = callCommand "stack build"
parse ("test":_)  = callCommand "stack test"
parse _           = buildAndExec

buildAndExec :: IO()
buildAndExec = do
  callCommand "stack build"
  startTime <- getClockTime
  callCommand "stack exec a"
  endTime <- getClockTime
  let (TimeDiff _ _ _ _ _ ss ps) = diffClockTimes endTime startTime
  let secs = fromIntegral ss + (fromIntegral ps / 1000000000000)
  putStrLn $ "Time Taken: " ++ show secs ++ " seconds"