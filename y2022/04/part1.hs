module Main where
  
import Control.Arrow
import Data.List
import Control.Applicative

import Debug.Trace

split :: (a -> Bool) -> [a] -> ([a],[a])
split p xs = 
  ( takeWhile p xs
  , case dropWhile p xs of
    []    -> []
    _:xs' -> xs')


splitAtDelim :: Eq a => a -> [a] -> ([a],[a])
splitAtDelim x = split (/= x)

readInt :: String -> Int
readInt = read

compute :: String -> Int
compute = lines
  >>> map (splitAtDelim ','
    >>> splitAtDelim '-' *** splitAtDelim '-'
    >>> (readInt *** readInt) *** (readInt *** readInt)
    >>> uncurry enumFromTo *** uncurry enumFromTo
    >>> liftA2 (||) (uncurry isSubsequenceOf) (uncurry (flip isSubsequenceOf))
    >>> fromEnum)
  >>> sum

main :: IO ()
main = (compute <$> getContents) >>= print