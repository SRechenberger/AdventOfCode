module Main where

import Text.Parsec
import Text.Parsec.String

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State as State
import Control.Arrow

import Data.List
import Data.Int

type Item = Int64

data Monkey = Monkey Int [Item] (Item -> Item) Item (Bool -> Int)

instance Show Monkey where
  show (Monkey x is _ _ _) = "Monkey " <> show x <> ": " <> show is 

startingItems :: Parser [Item]
startingItems = (read <$> many1 digit) `sepBy1` (string "," *> spaces)

operation :: Parser (Item -> Item)
operation = string "new" *> spaces *> string "=" *> spaces
  *> string "old" *> spaces
  *> (try (string "+") *> spaces
          *> ((try (string "old") *> pure (*2))
             <|> ((+) . read <$> many1 digit))
      <|> string "*" *> spaces
          *> ((try (string "old") *> pure (^2))
             <|> ((*) . read <$> many1 digit)))

test :: Parser Item
test = string "divisible" *> spaces *> string "by" *> spaces
  *> (read <$> many1 digit)  

next :: Parser (Bool -> Int)
next = do
  true <- string "If" *> spaces *> string "true" *> spaces *> string ":" *> spaces *> string "throw" *> spaces *> string "to" *> spaces *> string "monkey" *> spaces *> (read <$> many1 digit)
  newline >> spaces
  false <- string "If" *> spaces *> string "false" *> spaces *> string ":" *> spaces *> string "throw" *> spaces *> string "to" *> spaces *> string "monkey" *> spaces *> (read <$> many1 digit)
  pure $ \x -> case x of
    True -> true
    False -> false

monkey :: Parser Monkey
monkey = Monkey
  <$> (string "Monkey" *> spaces *> (read <$> many1 digit) <* spaces <* string ":")
  <*> (spaces *> string "Starting" *> spaces *> string "items" *> spaces *> string ":" *> spaces *> startingItems)
  <*> (spaces *> string "Operation" *> spaces *> string ":" *> spaces *> operation)
  <*> (spaces *> string "Test" *> spaces *> string ":" *> spaces *> test)
  <*> (spaces *> next)


throwTo :: Item -> Monkey -> Monkey
throwTo item (Monkey i items f p g) = Monkey i (items ++ [item]) f p g

turn :: Item -> Int -> State.State (Map Int Monkey) Int
turn r i = do
  Monkey _ xs f p g <- gets (Map.! i)
  forM_ xs $ \x -> do
    let x' = f x
    let x'' = mod x' r
    let j = g (x' `mod` p == 0)
    modify $ Map.adjust (throwTo x'') j
  modify $ Map.adjust (\(Monkey i _ f p g) -> Monkey i [] f p g) i
  pure $ length xs

round :: Item -> Map Int Int -> State.State (Map Int Monkey) (Map Int Int)
round r score = do
  m <- gets Map.size
  adjs <- forM [0..(m-1)] $ \i -> (i,) <$> turn r i
  pure $ foldr (\(k, s) -> Map.adjust (+s) k) score adjs

rounds :: Item -> Int -> Map Int Monkey -> Map Int Int -> Map Int Int
rounds _ 0 _  ss = ss
rounds r n ms ss = rounds r (n-1) ms' ss'
  where
    (ss', ms') = runState (Main.round r ss) ms

fromRight :: Either a b -> b
fromRight (Right x) = x

getPrime :: Monkey -> Item
getPrime (Monkey _ _ _ p _) = p

compute = parse (sepBy1 monkey spaces) ""
  >>> fromRight
  >>> map (\m@(Monkey i _ _ _ _) -> (i, m))
  >>> ((map (snd >>> getPrime) >>> product) &&& Map.fromList) &&& (map (second (const 0)) >>> Map.fromList)
  >>> uncurry (uncurry (flip rounds 10000))
  >>> Map.toList
  >>> map snd
  >>> sort
  >>> reverse
  >>> take 2
  >>> product


main :: IO ()
main = compute <$> getContents >>= print