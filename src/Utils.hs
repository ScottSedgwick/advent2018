module Utils where

import qualified Data.IntMap as M
import           Data.List (sortOn)

imsort :: Ord b => ((Int, a) -> b) -> M.IntMap a -> [(Int, a)]
imsort f = sortOn f . M.toList

immax :: Ord b => ((Int, a) -> b) -> M.IntMap a -> (Int, a)
immax f = last . imsort f

immin :: Ord b => ((Int, a) -> b) -> M.IntMap a -> (Int, a)
immin f = head . imsort f

invert :: M.IntMap Int -> M.IntMap [Int]
invert = M.foldrWithKey f M.empty
  where
    f k = M.alter (g k)
    g v' Nothing   = Just [v']
    g v' (Just vs) = Just (v':vs) 

-- alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a S
-- foldrWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b

-- invertOn :: (a -> Int) -> M.IntMap a -> M.IntMap [Int]
-- invertOn f m = M.foldrWithKey g M.empty m
--   where
--     g k a b        = M.alter (h k)                 -- (Key -> a -> b -> b)
--     h v Nothing   = Just [f v]
--     h v (Just vs) = Just (f v : vs)