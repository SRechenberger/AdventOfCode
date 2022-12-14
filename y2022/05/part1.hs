module Main where
  
import Text.Parsec
import Text.Parsec.String

import Control.Applicative
import Control.Arrow

import Data.List
import Data.Char

import Data.Map (Map)
import qualified Data.Map as Map

data Step = Move Int Int Int
  deriving Show


moveCrate :: Int -> Int -> Map Int String -> Map Int String
moveCrate from to stacks = case Map.lookup from stacks of
  Just (x:xs) -> Map.insert from xs
    $ Map.adjust (x:) to
    $ stacks

step :: Parser Step
step = Move
  <$> (string "move" *> spaces *> (read <$> many1 digit) <* spaces)
  <*> (string "from" *> spaces *> (read <$> many1 digit) <* spaces)
  <*> (string "to" *> spaces *> (read <$> many1 digit) <* eof)


parseStep :: String -> Step
parseStep s = case parse step "" s of
  Left e     -> error (show e)
  Right step -> step

uncons' = head &&& tail

readStacks = lines
  >>> takeWhile (/= "")
  >>> reverse
  >>> transpose
  >>> filter (head >>> isDigit)
  >>> map (takeWhile (/= ' '))
  >>> map uncons'
  >>> map (digitToInt *** reverse)
  >>> Map.fromList


readSteps = lines
  >>> dropWhile (/= "")
  >>> drop 1
  >>> map parseStep


processSteps :: [Step] -> Map Int String -> Map Int String
processSteps []              stacks = stacks
processSteps (Move n f t:ss) stacks = processSteps ss (move n f t stacks)
  where
    move 0 _ _ s = s
    move n f t s = move (n-1) f t $ moveCrate f t s


compute :: String -> String
compute = readSteps &&& readStacks
  >>> uncurry processSteps
  >>> Map.toList
  >>> map (snd >>> head)


main :: IO ()
main = (compute <$> getContents) >>= putStrLn