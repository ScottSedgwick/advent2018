module Utils where

import qualified Data.IntMap as M
import           Data.List (sortOn)

imsort :: Ord b => ((Int, a) -> b) -> M.IntMap a -> [(Int, a)]
imsort f = sortOn f . M.toList

immax :: Ord b => ((Int, a) -> b) -> M.IntMap a -> (Int, a)
immax f = last . imsort f

immin :: Ord b => ((Int, a) -> b) -> M.IntMap a -> (Int, a)
immin f = head . imsort f
