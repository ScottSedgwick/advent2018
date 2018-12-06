module Utils where

import qualified Data.IntMap as M
import           Data.List (sortOn)

imsort :: Ord b => ((Int, a) -> b) -> M.IntMap a -> [(Int, a)]
imsort f = sortOn f . M.toList

immax :: Ord b => ((Int, a) -> b) -> M.IntMap a -> (Int, a)
immax f = last . imsort f

immin :: Ord b => ((Int, a) -> b) -> M.IntMap a -> (Int, a)
immin f = head . imsort f

-- foldrWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
-- alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a
invert :: M.IntMap Int -> M.IntMap [Int]
invert = M.foldrWithKey f M.empty
  where
    f k = M.alter (g k)
    g v' Nothing   = Just [v']
    g v' (Just vs) = Just (v':vs) 