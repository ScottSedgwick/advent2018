module Day07 where

import Data.Char (chr, ord)
import Data.List (nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.IntSet as I
import qualified Data.IntMap as M

answer1 :: IO ()
answer1 = print $ map chr $ chain deps

chain :: M.IntMap I.IntSet -> [Int]
chain ms | null ms   = []
         | otherwise = x : chain ns
  where 
    x = fst $ head $ M.toList $ avail ms
    ns = removeall x ms

removeall :: Int -> M.IntMap I.IntSet -> M.IntMap I.IntSet
removeall x ms = zs
  where
    ys = M.delete x ms
    zs = M.map (I.delete x) ys

avail :: M.IntMap I.IntSet -> M.IntMap I.IntSet
avail = M.filter I.null

nodes :: I.IntSet
nodes = I.union heads tails

heads :: I.IntSet
heads = I.fromList $ nub $ map fst ds

tails :: I.IntSet
tails = I.fromList $ nub $ map snd ds

-- alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a
deps :: M.IntMap I.IntSet
deps = foldr (\(a,b) m -> M.alter (f a) b m) e ds
  where
    f x Nothing   = Just $ I.singleton x
    f x (Just xs) = Just $ I.insert x xs
    e = foldr (\k -> M.insert k I.empty) M.empty (I.toList nodes)


------------------------------------------------------------------

answer2 :: IO ()
answer2 = do
  res <- work workers [] 0 deps
  print res

work :: Int -> [(Int, Int)] -> Int -> M.IntMap I.IntSet -> IO Int
work wcount ws time dps 
  | null ws && M.null dps = pure time
  | otherwise = do
    putStrLn $ "Time: " ++ show time
    putStrLn $ "Initial Workers: " ++ show ws
    putStrLn $ "Initial deps: " ++ show dps
    putStrLn $ "======================================="
    work wcount nws time' dps''
  where 
    dtime  = if null ws then 0 else snd $ head $ sortOn snd ws
    ws'    = map (\(a,b) -> (a, b - dtime)) ws
    time'  = time + dtime
    done   = filter (\(a,b) -> b <= 0) ws'
    undone = filter (\(a,b) -> b > 0) ws'
    dps'   = foldr (\(a,b) m -> M.delete a m) dps done
    dps''  = M.map (\a -> foldr (\(b,c) d -> I.delete b d) a done) dps'
    dcs    = M.toList $ M.filterWithKey f dps''  -- need to filter from this list, the ones that are already in ws'
    f k a  = I.null a && not (k `I.member` (I.fromList $ map fst ws'))
    cs     = sortOn fst $ map (\(a,_) -> (a, steplen a)) dcs
    nws    = undone ++ take (wcount - length undone) cs



-- filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
------------------------------------------------------------------

steplen :: Int -> Int
steplen x = x - ord 'A' + 1 + 60

workers :: Int
-- workers = 2
workers = 5

ds :: [(Int, Int)]
-- ds = map (\(a,b) -> (ord a, ord b)) ds1
ds = map (\(a,b) -> (ord a, ord b)) ds2

type Dep = (Char, Char)

ds1 :: [Dep]
ds1 = 
  [ ('C', 'A')
  , ('C', 'F')
  , ('A', 'B')
  , ('A', 'D')
  , ('B', 'E')
  , ('D', 'E')
  , ('F', 'E')
  ]

ds2 = 
  [ ('G', 'X')
  , ('X', 'B')
  , ('A', 'I')
  , ('D', 'H')
  , ('O', 'T')
  , ('H', 'C')
  , ('S', 'E')
  , ('U', 'M')
  , ('M', 'Z')
  , ('R', 'N')
  , ('C', 'Q')
  , ('T', 'P')
  , ('I', 'W')
  , ('W', 'N')
  , ('P', 'J')
  , ('N', 'F')
  , ('Y', 'J')
  , ('J', 'L')
  , ('L', 'E')
  , ('E', 'B')
  , ('Q', 'B')
  , ('F', 'K')
  , ('V', 'K')
  , ('Z', 'B')
  , ('B', 'K')
  , ('G', 'U')
  , ('E', 'V')
  , ('A', 'Z')
  , ('C', 'V')
  , ('R', 'B')
  , ('Q', 'Z')
  , ('R', 'K')
  , ('T', 'B')
  , ('L', 'B')
  , ('M', 'K')
  , ('T', 'Z')
  , ('W', 'B')
  , ('I', 'E')
  , ('A', 'M')
  , ('V', 'Z')
  , ('Y', 'B')
  , ('Q', 'F')
  , ('W', 'Y')
  , ('U', 'K')
  , ('D', 'F')
  , ('P', 'F')
  , ('N', 'L')
  , ('H', 'T')
  , ('H', 'L')
  , ('C', 'T')
  , ('H', 'I')
  , ('Z', 'K')
  , ('L', 'Z')
  , ('Y', 'K')
  , ('I', 'V')
  , ('P', 'K')
  , ('P', 'N')
  , ('G', 'D')
  , ('I', 'J')
  , ('H', 'K')
  , ('L', 'Q')
  , ('D', 'M')
  , ('O', 'V')
  , ('R', 'L')
  , ('D', 'W')
  , ('M', 'J')
  , ('O', 'R')
  , ('N', 'Z')
  , ('Y', 'V')
  , ('W', 'L')
  , ('U', 'Y')
  , ('S', 'V')
  , ('M', 'P')
  , ('X', 'A')
  , ('A', 'E')
  , ('A', 'L')
  , ('A', 'R')
  , ('V', 'B')
  , ('P', 'B')
  , ('E', 'F')
  , ('T', 'V')
  , ('S', 'R')
  , ('T', 'F')
  , ('P', 'Y')
  , ('A', 'C')
  , ('J', 'F')
  , ('H', 'B')
  , ('C', 'E')
  , ('P', 'E')
  , ('D', 'I')
  , ('X', 'F')
  , ('T', 'Q')
  , ('J', 'B')
  , ('C', 'B')
  , ('P', 'Q')
  , ('H', 'R')
  , ('F', 'B')
  , ('T', 'J')
  , ('A', 'W')
  , ('N', 'K')
  , ('T', 'E')
  ]