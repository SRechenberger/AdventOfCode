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

compress :: Eq a => [a] -> [(a, Int)]
compress []     = []
compress (x:xs) = (x, length (takeWhile (==x) xs)) : compress (dropWhile (==x) xs)


bitFlip :: Char -> Char
bitFlip '0' = '1'
bitFlip '1' = '0'


compute = lines
  >>> transpose
  >>> map mostCommon
  >>> (,) <*> map bitFlip
  >>> (binary *** binary)
  >>> uncurry (*)


-- interactive :: FilePath -> IO ()
interactive f = compute <$> readFile f


main :: IO ()
main = compute <$> getContents >>= print