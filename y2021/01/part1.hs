module Main where

import Control.Arrow

readInt :: String -> Int
readInt = read

compute :: String -> Int
compute = lines
  >>> map readInt
  >>> zipWith (<) <*> tail
  >>> map fromEnum
  >>> sum

interactive :: FilePath -> IO ()
interactive f = compute <$> readFile f >>= print

main :: IO ()
main = compute <$> getContents >>= print