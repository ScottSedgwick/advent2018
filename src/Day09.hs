{-# LANGUAGE BangPatterns #-}
module Day09 where

import qualified Data.IntMap.Strict as I
import Data.Maybe (fromJust)
import Data.List (sortOn, foldl')
import qualified Data.List.PointedList as L
import qualified Data.List.PointedList.Circular as L
import Data.List.PointedList (deleteRight, focus, moveTo, replace)
import Data.List.PointedList.Circular (moveN, insert, next)
import Control.Lens

answer :: IO ()
answer = do
  let gs = foldl' turn initState initMarbles
  print $ winner gs

pcount :: Int
pcount = 429

mcount :: Int
mcount = 7090100

data GameState = GameState
  { ring    :: L.PointedList Int
  , scores  :: L.PointedList Int
  } deriving (Eq, Show)

winner :: GameState -> (Int, Int)
winner gs = last $ sortOn snd $ getScores $ scores gs

initMarbles :: [Int]
initMarbles = [3..mcount]

initRing :: [Int]
initRing = [0,2,1]

initCRing :: L.PointedList Int
initCRing = next $ fromJust $ L.fromList initRing

initScores :: [Int]
initScores = replicate pcount 0

initCScores :: L.PointedList Int
initCScores = next $ fromJust $ L.fromList initScores

initState :: GameState
initState = GameState
  { ring    = initCRing
  , scores  = initCScores
  } 

getScores :: L.PointedList Int -> [(Int, Int)]
getScores ls = f (length ls)
  where
    f 0 = []
    f n = (n, (fromJust $ moveTo (n - 1) ls)^.focus) : f (n - 1)

turn :: GameState -> Int -> GameState
turn !gs !m
  | m `mod` 23 == 0    = let !res = scoresTurn gs m in res
  | otherwise          = let !res = normalTurn gs m in res

normalTurn :: GameState -> Int -> GameState
normalTurn !gs !m = GameState {ring = rs, scores = ss}
  where
    rs = insert m $ moveN 1 (ring gs)
    ss = moveN 1 $ scores gs

scoresTurn :: GameState -> Int -> GameState
scoresTurn !gs !m = GameState {ring = qs, scores = ts}
  where
    rs = moveN (-7) $ ring gs
    s  = rs^.focus
    qs = fromJust $ deleteRight rs
    ss = moveN 1 $ scores gs
    t  = ss^.focus
    ts = replace (t + s + m) ss
