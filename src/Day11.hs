module Day11 where

import Data.List (sortOn)

answer :: IO ()
answer = do
  putStrLn "Day 11"
  print $ maxPwr 1309

maxPwr :: Int -> (Int, (Int, Int, Int))
maxPwr serial = last pwrs
  where
    pwrs = sortOn fst $ map (sqPwr serial) [(x,y,s) | s <- [1..25], x <- [1..(300 - s + 1)], y <- [1..(300 - s + 1)]]

sqPwr :: Int -> (Int, Int, Int) -> (Int, (Int, Int,Int))
sqPwr serial (x,y,size) = (p,(x,y,size))
  where
    p = foldr (\a b -> b + power serial a) 0 cs
    cs = [(x',y') | x' <- [x..x + size - 1], y' <- [y..y + size - 1]]

power :: Int -> (Int, Int) -> Int
power serial (x, y) = hunds - 5
  where
    rackId = x + 10
    posnN  = rackId * y + serial
    power  = posnN * rackId
    hunds  = (power `div` 100) `mod` 10
