module Main where
  
import Control.Applicative 
import Data.List

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p xs = takeWhile p xs : case dropWhile p xs of
  []    -> []
  _:xs' -> split p xs'


compute :: String -> Int
compute = sum 
  . take 3 
  . reverse 
  . sort 
  . map sum 
  . map (map read) 
  . split (/="") 
  . lines 


main :: IO ()
main = (compute <$> getContents) >>= print
  