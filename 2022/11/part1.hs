{-# LANGUAGE TupleSections #-}

import Text.Parsec
import Text.Parsec.String

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State as State
import Control.Arrow

import Data.List
import Data.Function


data Monkey = Monkey Int [Int] (Int -> Int) (Int -> Bool) (Bool -> Int)

instance Show Monkey where
  show (Monkey x is _ _ _) = "Monkey " <> show x <> ": " <> show is 

startingItems :: Parser [Int]
startingItems = (read <$> many1 digit) `sepBy1` (string "," *> spaces)

operation :: Parser (Int -> Int)
operation = string "new" *> spaces *> string "=" *> spaces
  *> string "old" *> spaces
  *> (try (string "+") *> spaces
          *> ((try (string "old") *> pure (*2))
             <|> ((+) . read <$> many1 digit))
      <|> string "*" *> spaces
          *> ((try (string "old") *> pure (^2))
             <|> ((*) . read <$> many1 digit)))

test :: Parser (Int -> Bool)
test = string "divisible" *> spaces *> string "by" *> spaces
  *> ((\y x -> x `mod` y == 0) . read <$> many1 digit)  

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


throwTo :: Int -> Monkey -> Monkey
throwTo item (Monkey i items f p g) = Monkey i (items ++ [item]) f p g

turn :: Int -> State.State (Map Int Monkey) Int
turn i = do
  Monkey _ xs f p g <- gets (Map.! i)
  forM_ xs $ \x -> do
    let x' = f x `div` 3
    let j = g (p x')
    modify $ Map.adjust (throwTo x') j
  modify $ Map.adjust (\(Monkey i _ f p g) -> Monkey i [] f p g) i
  pure $ length xs

round :: Map Int Int -> State.State (Map Int Monkey) (Map Int Int)
round score = do
  m <- gets Map.size
  adjs <- forM [0..(m-1)] $ \i -> (i,) <$> turn i
  pure $ foldr (\(k, s) -> Map.adjust (+s) k) score adjs

rounds :: Int -> Map Int Monkey -> Map Int Int -> Map Int Int
rounds 0 _  ss = ss
rounds n ms ss = rounds (n-1) ms' ss'
  where
    (ss', ms') = runState (Main.round ss) ms

fromRight :: Either a b -> b
fromRight (Right x) = x

compute = parse (sepBy1 monkey spaces) ""
  >>> fromRight
  >>> map (\m@(Monkey i _ _ _ _) -> (i, m))
  >>> Map.fromList &&& (map (second (const 0)) >>> Map.fromList)
  >>> uncurry (rounds 20)
  >>> Map.toList
  >>> map snd
  >>> sort
  >>> reverse
  >>> take 2
  >>> product


main :: IO ()
main = compute <$> getContents >>= print