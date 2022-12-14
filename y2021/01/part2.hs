module Main where

import Control.Arrow

readInt :: String -> Int
readInt = read

compute :: String -> Int
compute = lines
  >>> map readInt
  >>> zipWith3 (\x y z -> x+y+z) <$> id <*> tail <*> (tail >>> tail)
  >>> zipWith (<) <*> tail
  >>> map fromEnum
  >>> sum

interactive :: FilePath -> IO ()
interactive f = compute <$> readFile f >>= print

main :: IO ()
main = compute <$> getContents >>= print