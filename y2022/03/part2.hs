module Main where
  
import Control.Arrow
import Control.Applicative
import Data.Maybe
import Data.List

prio :: Char -> Int
prio c | 'a' <= c && c <= 'z' = fromEnum c - fromEnum 'a' + 1
       | 'A' <= c && c <= 'Z' = fromEnum c - fromEnum 'A' + 27
       | otherwise = error "Fuck..."


chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks i xs
  | i <= length xs = take i xs : chunks i (drop i xs)
  | otherwise      = [xs]


compute :: String -> Int
compute = lines
  >>> chunks 3
  >>> map
    (foldr1 intersect
    >>> head
    >>> prio)
  >>> sum

main :: IO ()
main = (compute <$> getContents) >>= print
