module Main where

import Control.Arrow

data Command 
  = Forward
  | Down
  | Up
  deriving (Show, Eq)


command :: String -> (Command, Int)
command s = case takeWhile (/=' ') s of
    "forward" -> (Forward, i)
    "down"    -> (Down, i)
    "up"      -> (Up, i)
  where
    i = dropWhile (/=' ') >>> tail >>> read $ s 


type Position = (Int, Int)


move :: Position -> (Command, Int) -> Position
move (x,d) (c, i) = case c of
  Forward -> (x+i, d)
  Up      -> (x, d-i)
  Down    -> (x, d+i)


compute = lines
  >>> map command
  >>> foldl move (0,0)
  >>> uncurry (*)


interactive :: FilePath -> IO ()
interactive f = compute <$> readFile f >>= print


main :: IO ()
main = compute <$> getContents >>= print