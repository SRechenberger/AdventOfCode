module Main where

import Data.List (find)
import Data.Maybe (fromJust)

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


isNoDistressBeacon :: Point -> Point -> Point -> Bool
isNoDistressBeacon s b p = dist s p <= dist s b


outerRim :: Point -> Point -> [Point]
outerRim s@(x,y) b = diag south east <> diag east north <> diag north west <> diag west south
  where
    d = dist s b
    south = (x+d+1,y)
    north = (x-d-1,y)
    east = (x,y+d+1)
    west = (x,y-d-1)


diag :: Point -> Point -> [Point]
diag (x1,y1) (x2,y2) = zip [x1,(x1+signum (x2-x1))..x2] [y1,(y1+signum (y2-y1))..y2]


toList :: (a,a) -> [a]
toList (a,b) = [a,b]


compute m = lines
  >>> map readLine
  >>> map ((,) <$> uncurry outerRim <*> uncurry isNoDistressBeacon)
  >>> unzip
  >>> first concat
  >>> first (filter (\(x,y) -> 0 <= x && x <= m && 0 <= y && y <= m))
  >>> second (foldr1 (liftA2 (||)))
  >>> second (not .)
  >>> uncurry (flip filter)
  >>> head
  >>> first (*4000000)
  >>> uncurry (+)


interactive :: Int -> FilePath -> IO Int
interactive m f = compute m <$> readFile f


main :: IO ()
main = compute 4000000 <$> getContents >>= print