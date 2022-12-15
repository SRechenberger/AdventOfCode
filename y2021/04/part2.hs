{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.Parsec
import Text.Parsec.String

import Control.Lens
import Control.Arrow

import Data.List
import Data.Either
import Data.Function


swap (x,y) = (y,x)


draws :: String -> [Int]
draws s = case parse (sepBy1 (read <$> many1 digit) (string ",")) "" s of
  Left  e -> error (show e)
  Right is -> is


sepAt :: Eq a => a -> [a] -> [[a]]
sepAt _   [] = []
sepAt sep xs = ys : sepAt sep zs
  where
    (ys,zs) = case span (/= sep) xs of
      (ys, []) -> (ys, [])
      (ys, _:zs) -> (ys, zs)


data Board = Board
  { _board    :: [[Int]]
  , _unmarked :: [Int]
  , _marked   :: [Int]
  }
  deriving (Show, Eq)

makeLenses ''Board

mkBoard :: [[Int]] -> Board
mkBoard bss = Board bss (concat bss) []

bingo :: Board -> Bool
bingo b = any (all (`elem` (b^.marked))) $ (b^.board) ++ (b^.board.to transpose)

mark :: Int -> Board -> Board
mark i b = b
  & unmarked %~ delete i
  & marked %~ (i:)

score :: Board -> Int
score b = sum (b^.unmarked) * b^.marked.to head


play :: [Int] -> Board -> Either (Int, Int) Board
play [] b = Right b
play (x:xs) b
  | bingo b'  = Left (score b', length xs)
  | otherwise = play xs b'
  where
    b' = mark x b


compute = lines
  >>> (,) <$> head <*> drop 2
  >>> first (sepAt ',')
  >>> first (map read)
  >>> first play
  >>> second (sepAt "")
  >>> second (map (map (words >>> map read)))
  >>> second (map mkBoard)
  >>> uncurry map
  >>> lefts
  >>> minimumBy (compare `on` snd)
  >>> fst



interactive f = compute <$> readFile f


main :: IO ()
main = compute <$> getContents >>= print