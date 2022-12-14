module Main where

import Control.Arrow

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.String

import Debug.Trace


type Point = (Int, Int)
type Path = [Point]


point :: Parser Point
point = (,)
  <$> (read <$> (many1 digit <* string ","))
  <*> (read <$> many1 digit)

path :: Parser Path
path = sepBy1 point (spaces *> string "->" *> spaces)


parsePath :: String -> Path
parsePath s = case parse path "" s of
  Left e   -> error (show e)
  Right ps -> ps


drawPath :: Point -> Point -> Path
drawPath (x,y) (x',y')
  | x == x' = [(x,iy) | iy <- [min y y'..max y y']]
  | y == y' = [(ix,y) | ix <- [min x x'..max x x']]


completePath :: Path -> Set Point
completePath [] = Set.empty
completePath (p1:p2:ps) = Set.fromList (drawPath p1 p2) `Set.union` case ps of
  []  -> Set.empty
  ps' -> completePath (p2:ps)


simulateSand :: Set Point -> Int -> Point -> Maybe (Set Point)
simulateSand set floor = sim
  where
    sim p@(x,y)
      | Set.member p set            = Nothing
      | y+1 >= floor                = Just $ Set.insert p set
      | Set.notMember (x,  y+1) set = sim (x,y+1)
      | Set.notMember (x-1,y+1) set = sim (x-1,y+1)
      | Set.notMember (x+1,y+1) set = sim (x+1,y+1)
      | otherwise                   = Just $ Set.insert p set


simulate :: Point -> Set Point -> Int
simulate source set = sim set
  where
    floor = maximum [ y | (_,y) <- Set.toList set ] + 2
    sim set = case simulateSand set floor source of
      Nothing   -> 0
      Just set' -> 1 + sim set' 


compute = lines
  >>> map parsePath
  >>> map completePath
  >>> Set.unions
  >>> simulate (500,0)


interactive :: FilePath -> IO ()
interactive f = compute <$> readFile f >>= print


main :: IO ()
main = compute <$> getContents >>= print