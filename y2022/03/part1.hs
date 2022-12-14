module Main where
  
import Control.Arrow
import Control.Applicative
import Data.Maybe
import Data.List

prio :: Char -> Int
prio c | 'a' <= c && c <= 'z' = fromEnum c - fromEnum 'a' + 1
       | 'A' <= c && c <= 'Z' = fromEnum c - fromEnum 'A' + 27
       | otherwise = error "Fuck..."

compute :: String -> Int
compute = lines
  >>> map
    (flip splitAt <*> (length >>> (`div` 2))
    >>> liftA2 find (fst >>> flip elem) snd
    >>> fromJust
    >>> prio)
  >>> sum

main :: IO ()
main = (compute <$> getContents) >>= print
