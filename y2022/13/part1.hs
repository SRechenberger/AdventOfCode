module Main where

import Text.Parsec
import Text.Parsec.String

import Control.Arrow

data Packet
  = Singleton Int
  | List [Packet]
  deriving (Show, Eq)

instance Ord Packet where
  Singleton x     <= Singleton y     = x <= y
  s@(Singleton _) <= l@(List _)      = List [s] <= l
  l@(List _)      <= s@(Singleton _) = l <= List [s]
  List []         <= List _          = True
  List (_:_)      <= List []         = False
  List (x:xs)     <= List (y:ys)     
    | x < y     = True
    | x == y    = List xs <= List ys
    | otherwise = False


singleton :: Parser Packet
singleton = Singleton . read <$> many1 digit

list :: Parser Packet
list = List <$> (string "[" *> sepBy packet (string ",") <* string "]")

packet :: Parser Packet
packet = try list <|> singleton

pairs :: [String] -> [(String, String)]
pairs [] = []
pairs xs = (x,y) : case xs' of
    []     -> []
    _:xs'' -> pairs xs''
  where
    ([x,y], xs') = span (not . null) xs

parsePacket :: String -> Packet
parsePacket s = case parse packet "" s of
  Right p -> p
  Left  e -> error (show e)

compute = lines
  >>> pairs
  >>> map (parsePacket *** parsePacket)
  >>> zip [1..]
  >>> filter (uncurry (<=) . snd)
  >>> map fst
  >>> sum

interactive :: FilePath -> IO ()
interactive f = compute <$> readFile f >>= print

main :: IO ()
main = compute <$> getContents >>= print