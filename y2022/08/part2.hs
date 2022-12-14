module Main where

import Control.Arrow

import Data.List
import Data.Char

visible :: [Int] -> [Int]
visible = zip <*> (inits >>> map reverse)
  >>> map (uncurry cast)

cast :: Int -> [Int] -> Int
cast _ [] = 0
cast t (t':ts)
  | t' < t    = 1 + cast t ts
  | otherwise = 1

visibleFrom :: Int -> [[Int]] -> [[Int]]
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
  >>> foldr1 (zipWith (zipWith (*)))
  >>> concat
  >>> maximum


main :: IO ()
main = (compute <$> getContents) >>= print
