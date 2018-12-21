{-# LANGUAGE OverloadedLists #-}
module Day12 where

import Language.Literals.Binary
import qualified Data.IntSet as I
import Data.Sequence ((<|), (|>), (><))
import qualified Data.Sequence as Seq

data PotState = PotState 
  { offset :: Int
  , gen :: Int
  , pots :: Seq.Seq Int
  } deriving (Show, Eq)

answer :: IO ()
answer = do
  putStrLn "Day 12"
  putStrLn "---------------------------------------------------------------------------"
  answer1
  putStrLn "---------------------------------------------------------------------------"
  answer2


answer1 :: IO ()
answer1 = do
  let gs = generation ids 20
  let s = scoreSet gs
  putStrLn $ "After 20 generations: " ++ show s
  putStrLn $ "(Should be " ++ show ans ++ ")"

generation :: PotState -> Int -> PotState
generation ps n 
  | n == gen ps = ps
  | otherwise   = generation (step ps) n

step :: PotState -> PotState
step ps = PotState { offset = offset ps - 1, gen = gen ps + 1, pots = fmap gentrans (zip5 $ (0 <| pots ps) |> 0)}

gentrans :: Int -> Int
gentrans x = if x `I.member` ds then 1 else 0

zip5 :: Seq.Seq Int -> Seq.Seq Int
zip5 xs = [0,0] >< z xs
  where
    z xs | length xs >= 5 = readBin (Seq.take 5 xs) <| z (Seq.drop 1 xs)
         | otherwise      = [0,0]

readBin :: Seq.Seq Int -> Int
readBin = foldl (\a b -> a * 2 + b) 0

scoreSet :: PotState -> Int
scoreSet ps = sum $ fmap (uncurry (*)) xs
  where
    ss = pots ps
    x  = offset ps
    xs = Seq.zip ss [x .. Seq.length ss]

answer2 :: IO ()
answer2 = do
  let g = 50000000000 - 108
  let x = g * 65
  let s = x + 7976
  putStrLn $ "Answer2: " ++ show s

signature :: Seq.Seq Int -> Int
signature = foldr (\a b -> b * 2 + a) 0 . trimZeros

findRepeat :: I.IntSet -> PotState -> IO (Int, Int)
findRepeat ss ps = do
  let s = signature (pots ps)
  if (s == 6923733619186535179) || s `I.member` ss
    then pure (gen ps, s)
    else findRepeat (I.insert s ss) (step ps)

trimZeros :: Seq.Seq Int -> Seq.Seq Int
trimZeros = Seq.dropWhileL (==0) . Seq.dropWhileR (==0)

-----------------------------------------------------------------------------------------------------------------
-- Data.

ids :: PotState
ds :: I.IntSet

-- ids = let (x, xs) = ids1 in PotState { offset = x, gen = 0, pots = Seq.fromList $ map (\c -> read [c] :: Int) xs }
-- ds  = ds1
-- ans = ans1
ids = let (x, xs) = ids2 in PotState { offset = x, gen = 0, pots = Seq.fromList $ map (\c -> read [c] :: Int) xs }
ds  = ds2
ans = ans2

ans1 = 325
ans2 = 2917

ids1 :: (Int, String)
ids1 = ((-3), "000100101001100000011100011100000000000")

ids2 :: (Int, String)
ids2 = (0, "1000000011011101010011001100101011100111001101010011000011111001101000001111111100001000011010011000")

ds1 :: I.IntSet
ds1 = [3,4,8,10,11,12,15,21,23,26,27,28,29,30]

ds2 :: I.IntSet
ds2 = [2,5,8,11,13,14,17,18,21,24,25,26,29,30,31]
