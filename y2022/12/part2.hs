module Main where

import Control.Arrow

import Data.Map (Map, (!?), (!))
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List
import Data.Maybe
import Data.Function

import Data.Function

import Control.Monad.State

import Debug.Trace


type Node = (Int, Int)
type Alt = Int

assoc (a,(b,c)) = ((a,b),c)
swap (a,b) = (b,a)

both :: (a -> b) -> (a,a) -> (b,b)
both = (***) <*> id

alt :: Char -> Int
alt 'E' = fromEnum 'z' - fromEnum 'a'
alt 'S' = fromEnum 'a' - fromEnum 'a'
alt x   = fromEnum x - fromEnum 'a'

(+#) :: Node -> Node -> Node
(x,y) +# (a,b) = (x+a,y+b)

infixr 8 .:
(.:) = (.) . (.)

edges :: Map Node Int -> Node -> [Node]
edges graph node =
  [ node +# d
  | d <- [(-1,0),(1,0),(0,-1),(0,1)]
  , Just h' <- [graph !? (node +# d)]
  , Just h <- [graph !? node]
  , h' - h <= 1
  ]

shortestPath :: Node -> Node -> Map Node Int -> Maybe Int
shortestPath source dest graph = shortestPath' Set.empty (Map.singleton source 0) [(0, source)] !? dest
  where
    shortestPath' _            distanceTable []    = distanceTable
    shortestPath' visitedNodes distanceTable queue = case sortBy (compare `on` fst) queue of
      (d,n):queue'
        | Set.member n visitedNodes -> shortestPath' visitedNodes distanceTable queue'
        | otherwise                 -> let
            neighbors = edges graph n
            distanceTable' = foldr (Map.alter (Just . maybe (d+1) (min (d+1)))) distanceTable neighbors
            visitedNodes' = Set.insert n visitedNodes
          in shortestPath' visitedNodes' distanceTable' ([(distanceTable' ! n, n) | n <- neighbors] ++ queue')
      
      

compute = lines
  >>> zip [1..]
  >>> map (uncurry $ map . (,))
  >>> map (zip [1..]) 
  >>> map (map assoc)
  >>> concat
  >>> (,) <*> ((,) <$> filter ((`elem` "aS") . snd) <*> find ((=='E') . snd))
  >>> first Map.fromList
  >>> first (fmap alt)
  >>> swap
  >>> first (second (fromJust >>> fst))
  >>> first (first (map fst))
  >>> (\((ss, d), g) -> map (\s -> shortestPath s d g) ss)
  >>> catMaybes
  >>> minimum

main :: IO ()
main = compute <$> getContents >>= print