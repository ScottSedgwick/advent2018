module Day6 where

import qualified Data.List as L
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as I
import Utils

answer1 :: IO ()
answer1 = print $ length $ snd $ immax (length . snd) candidates

type Point = (Int, Int)

encodePt :: Point -> Int
encodePt (x,y) = x * 1000 + y

decodePt :: Int -> Point
decodePt n = (x,y)
  where
    x = n `div` 1000
    y = n `mod` 1000

dpts :: M.IntMap Int
dpts = M.fromList $ map nearest pts

nearest :: Int -> (Int, Int)
nearest p = (p, p')
  where 
    p' = findNearest 0 1000 p ds

edges :: [Int]
edges = map encodePt $ 
  [(x,1)   | x <- [1..350]] ++ 
  [(x,350) | x <- [1..350]] ++
  [(1,y)   | y <- [1..350]] ++
  [(350,y) | y <- [1..350]] 

infinites :: [Int]
infinites = nullPt : I.toList (foldr f I.empty edges)
  where
    f e ps = case M.lookup e dpts of
               Nothing  -> ps
               (Just p) -> I.insert p ps

candidates :: M.IntMap [Int]
candidates = foldr M.delete (invert dpts) infinites

nullPt :: Int
nullPt = -1

findNearest :: Int -> Int -> Int -> [Int] -> Int
findNearest mp md p1 [] = mp
findNearest mp md p1 (p:ps)  
  | d == md   = nullPt
  | d < md    = findNearest p d p1 ps 
  | otherwise = findNearest mp md p1 ps
  where
    d = distance p1 p 


distance :: Int -> Int -> Int
distance p1 p2 = abs (a - c) + abs (b - d)
  where
    (a,b) = decodePt p1
    (c,d) = decodePt p2


-- Map across pts, and generate an list (Int, Int) to the sum of the distances from p to all points in ds
-- Filter that list to only ones where the distance sum is less than the threshold
answer2 :: IO ()
answer2 = print $ length $ filter (\(_,d) -> d < threshold) $ dists pts

mapsize :: Int
mapsize = 350
-- mapsize = 10

threshold :: Int
threshold = 10000
-- threshold = 32

ds :: [Int]
ds = map encodePt ds2
-- ds = map encodePt ds1

pts :: [Int]
pts = map encodePt [(x,y) | x <- [1..mapsize], y <- [1..mapsize]]

dists :: [Int] -> [(Int, Int)]
dists [] = []
dists (p:ps) = if d > threshold
               then dists ps
               else (p,d) : dists ps
  where
    d = sumdist 0 p ds

sumdist :: Int -> Int -> [Int] -> Int
sumdist sd _ [] = sd
sumdist sd p (x:xs) 
  | sd > threshold = sd
  | otherwise      = sumdist (sd + distance p x) p xs


ds1 :: [Point]
ds1 = 
  [ (1, 1)
  , (1, 6)
  , (8, 3)
  , (3, 4)
  , (5, 5)
  , (8, 9)
  ]

ds2 = 
  [ (66, 204)
  , (55, 226)
  , (231, 196)
  , (69, 211)
  , (69, 335)
  , (133, 146)
  , (321, 136)
  , (220, 229)
  , (148, 138)
  , (42, 319)
  , (304, 181)
  , (101, 329)
  , (72, 244)
  , (242, 117)
  , (83, 237)
  , (169, 225)
  , (311, 212)
  , (348, 330)
  , (233, 268)
  , (99, 301)
  , (142, 293)
  , (239, 288)
  , (200, 216)
  , (44, 215)
  , (353, 289)
  , (54, 73)
  , (73, 317)
  , (55, 216)
  , (305, 134)
  , (343, 233)
  , (227, 75)
  , (139, 285)
  , (264, 179)
  , (349, 263)
  , (48, 116)
  , (223, 60)
  , (247, 148)
  , (320, 232)
  , (60, 230)
  , (292, 78)
  , (247, 342)
  , (59, 326)
  , (333, 210)
  , (186, 291)
  , (218, 146)
  , (205, 246)
  , (124, 204)
  , (76, 121)
  , (333, 137)
  , (117, 68)
  ]