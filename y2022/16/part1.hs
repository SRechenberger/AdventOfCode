{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Arrow
import Control.Applicative
import Control.Lens

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe
import Data.List
import Data.Function

import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Debug.Trace

data Room = Room 
  { _rate  :: Int
  , _connections :: [String]
  }
  deriving (Show, Eq)

makeLenses ''Room


readLine :: String -> (String, Room)
readLine s = (l, Room (read r) cs)
  where
    l:cs = getAllTextMatches (s =~ "[A-Z]{2}")
    r = s =~ "[0-9]+"


fuckit :: Eq a => Iso' (Maybe a) a
fuckit = non undefined


totalRate :: Map String Room -> Set String -> Int
totalRate graph = Set.foldr (+) 0 . Set.map ((graph !) >>> _rate)


shortestPaths :: Map String [String] -> String -> Map String Int
shortestPaths graph source = shortestPath' Set.empty (Map.singleton source 0)  [(0, source)]
  where
    shortestPath' _       table []    = table
    shortestPath' visited table queue = case sortBy (compare `on` fst) queue of
      (d,n):queue'
        | Set.member n visited -> shortestPath' visited table queue'
        | otherwise            -> let
            neighbors = graph ! n
            table' = foldr (Map.alter (Just . maybe (d+1) (min (d+1)))) table neighbors
            visited' = Set.insert n visited
            queue'' = [(table' ! n, n) | n <- neighbors] <> queue'
          in shortestPath' visited' table' queue''


-- search :: Map String Room -> Int
search rooms = search' 30 "AA" []
  where
    graph = fmap _connections rooms 

    search' 0 _ _ = 0
    search' t c v = if null cs then 0 else maximum [ val + search' (t - cost) next (next:v) | ((next, cost), val) <- cs ]
      where
        cs = c
          & shortestPaths graph
          & Map.toList
          & filter (\(r, i) -> i < t && r `notElem` v)
          & map (\(r, i) -> ((r, i+1), _rate (rooms ! r) * (t - (i+1))))
          & filter (\(_, v) -> v > 0)


compute = lines
  >>> map readLine
  >>> Map.fromList
  >>> search


-- interactive :: FilePath -> IO Int
interactive f = compute <$> readFile f >>= print


main :: IO ()
main = compute <$> getContents >>= print