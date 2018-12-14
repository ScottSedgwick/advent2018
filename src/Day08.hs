{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Day08 where

import Control.Monad.State
import Data.List.Split (splitOn)
import Day08Data

answer1 :: IO ()
answer1 = do
  (ts, xs) <- build 1 ds
  print $ summeta ts

data Tree = Tree ([Tree], [Int]) deriving (Eq, Show)

build :: Int -> [Int] -> IO ([Tree], [Int])
build _ [] = pure ([],[])
build 0 xs = pure ([],xs)
build n (a:b:xs) = do
  (ts, ys) <- build a xs
  let ms = take b ys
  let zs = drop b ys
  let t = Tree (ts, ms)
  (ts', xs') <- build (n - 1) zs
  pure (t:ts', xs')

summeta :: [Tree] -> Int
summeta [] = 0
summeta (Tree(t,m):ts) = sum m + summeta t + summeta ts


answer2 :: IO ()
answer2 = do
  (ts, xs) <- build 1 ds
  print $ value $ head ts

value :: Tree -> Int
value (Tree (ts,ms)) 
  | null ts   = sum ms
  | otherwise = sum $ map f ms
  where f n = if n <= length ts then value (ts !! (n - 1)) else 0


ds :: [Int]
ds = ds2

ds1 :: [Int]
ds1 = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
