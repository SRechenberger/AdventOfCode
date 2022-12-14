module Main where
  
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List


compute :: String -> Int
compute = (zipWith4 (\a b c d -> length (nub [a,b,c,d]))
    <$> id
    <*> drop 1
    <*> drop 2
    <*> drop 3)
  >>> findIndex (== 4)
  >>> fromJust
  >>> (+4)

main :: IO ()
main = (lines >>> map compute) <$> getContents >>= mapM_ print