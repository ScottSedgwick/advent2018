module Utils where

import qualified Data.IntMap as M
import           Data.List (sortOn)

ismax :: Ord b => ((Int, a) -> b) -> M.IntMap a -> (Int, a)
ismax f = last . sortOn f . M.toList
