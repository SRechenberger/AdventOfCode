module Main where

import Control.Arrow

import Data.List
import Data.Function

binary :: String -> Int
binary = foldl (\a b -> 2*a + b2i b) 0
  where
    b2i '0' = 0
    b2i '1' = 1


mostCommon :: Ord a => [a] -> a
mostCommon = sort >>> compress >>> maximumBy (compare `on` snd) >>> fst

leastCommon :: Ord a => [a] -> a
leastCommon = sort >>> compress >>> minimumBy (compare `on` snd) >>> fst

compress :: Eq a => [a] -> [(a, Int)]
compress []     = []
compress (x:xs) = (x, length (takeWhile (==x) xs)) : compress (dropWhile (==x) xs)


bitFlip :: Char -> Char
bitFlip '0' = '1'
bitFlip '1' = '0'


oxygen :: [String] -> String
oxygen xs | [] `elem` xs = []
oxygen xs                = b : oxygen [ys | y:ys <- xs, y == b]
  where
    b = mostCommon (map head xs)


co2scrubber :: [String] -> String
co2scrubber xs | [] `elem` xs = []
co2scrubber xs                = b : co2scrubber [ys | y:ys <- xs, y == b]
  where
    b = leastCommon (map head xs)


compute = lines
  >>> (,) <$> oxygen <*> co2scrubber
  >>> (binary *** binary)
  >>> uncurry (*)


-- interactive :: FilePath -> IO ()
interactive f = compute <$> readFile f


main :: IO ()
main = compute <$> getContents >>= print