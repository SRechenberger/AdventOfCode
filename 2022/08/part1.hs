{-# LANGUAGE TupleSections #-}

import Control.Arrow

import Data.List
import Data.Char

visible :: [Int] -> [Bool]
visible = map (True,)
  >>> scanl1 (\(_,x) (_,x') -> (x<x', max x x'))
  >>> map fst

visibleFrom :: Int -> [[Int]] -> [[Bool]]
visibleFrom n = rot n >>> map visible >>> rot' n

rotate = transpose >>> reverse
rotate' = reverse >>> transpose

rot 0 = id
rot n = rotate >>> rot (n-1)

rot' 0 = id
rot' n = rotate' >>> rot' (n-1)



compute = lines
  >>> map (map digitToInt)
  >>> map <$> flip visibleFrom <*> const [1..4]
  >>> foldr1 (zipWith (zipWith (||)))
  >>> concat
  >>> filter id
  >>> length


main :: IO ()
main = (compute <$> getContents) >>= print
