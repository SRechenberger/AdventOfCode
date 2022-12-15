module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow
import Control.Applicative

import Text.Regex.TDFA
import Text.Regex.TDFA.Text


type Point = (Int, Int)


readLine :: String -> (Point, Point)
readLine line = ((read xs, read ys), (read xb, read yb))
  where
    coordRegex = "-{0,1}[0-9]+"
    [xs,ys,xb,yb] = getAllTextMatches (line =~ coordRegex)


dist :: Point -> Point -> Int
dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)


isNoBeacon :: Point -> Point -> Point -> Bool
isNoBeacon s b p 
  | p == b = False
  | otherwise = dist s p <= dist s b


ranges :: Point -> Point -> (Int, Int)
ranges s@(x,_) b = (x - dist s b, x + dist s b) 


toList :: (a,a) -> [a]
toList (a,b) = [a,b]


compute l = lines
  >>> map readLine
  >>> (,) <*> map (uncurry isNoBeacon)
  >>> second (foldr1 (liftA2 (||)))
  >>> first (map (uncurry ranges))
  >>> first unzip
  >>> first (minimum *** maximum)
  >>> first (uncurry enumFromTo >>> flip zip (cycle [l]))
  >>> uncurry (flip filter)
  >>> length

interactive :: Int -> FilePath -> IO Int
interactive l f = compute l <$> readFile f


main :: IO ()
main = compute 2000000 <$> getContents >>= print