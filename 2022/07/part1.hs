import Data.Map (Map, (!))
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.String

import Control.Arrow

import Data.Either

data Directory 
  = Folder (Map String Directory)
  | File Int
  deriving (Show, Eq)

foldDirectory :: (Map String a -> a) -> (Int -> a) -> Directory -> a
foldDirectory folder file = fold
  where
    fold (Folder m) = folder (fold <$> m)
    fold (File s)   = file s

data ChangedDir = ChangedDir String (Map String Directory)
  deriving (Show, Eq)

type Zipper = (Directory, [ChangedDir])

descend :: String -> Zipper -> Zipper
descend p (Folder m, cs) = (m ! p, ChangedDir p (Map.delete p m) : cs)

ascend :: Zipper -> Zipper
ascend (t, ChangedDir p m : cs) = (Folder $ Map.insert p t m, cs)
ascend (t, [])                  = error $ "Cant ascend from " ++ show t

test = Folder $ Map.fromList 
  [ ("a", Folder $ Map.fromList
    [ ("e", Folder $ Map.fromList 
      [ ("i", File 584) ] )
    , ("f", File 29116)
    , ("g", File 2557)
    , ("h.lst", File 62596) ])
  , ("b.txt", File 14848514)
  , ("c.dat", File 8504156)
  , ("d", Folder $ Map.fromList 
    [ ("j", File 4060174)
    , ("d.log", File 8033020)
    , ("d.ext", File 5626152)
    , ("k", File 7214296) ])]

data Console 
  = CD String
  | LS
  | DIR String
  | FILE Int String
  deriving (Eq, Show)

console :: Parser Console
console = try command <|> output -- <?> "console"

command, cd, ls :: Parser Console
command = do
  string "$"
  spaces
  try cd <|> ls <?> "command"

cd = CD <$> (string "cd" *> spaces *> many1 anyChar)

ls = const LS <$> (string "ls")

output, dir, file :: Parser Console
output = try dir <|> file <?> "output"

dir = DIR <$> (string "dir" *> spaces *> many1 anyChar)

file = FILE <$> (read <$> many1 digit) <*> (spaces *> many1 anyChar)

apply :: Console -> Zipper -> Zipper
apply (DIR d)    (Folder m, cs) = (Folder $ Map.insert d (Folder Map.empty) m, cs)
apply (FILE s n) (Folder m, cs) = (Folder $ Map.insert n (File s) m, cs)
apply (CD "..")  z              = ascend z
apply (CD "/")   z              = z
apply (CD p)     z              = descend p z 
apply LS         z              = z

top :: Zipper -> Directory
top (d, []) = d
top z       = top $ ascend z

compute = lines
  >>> map (parse console "")
  >>> rights
  >>> foldl (flip apply) (Folder $ Map.empty, [])
  >>> top
  >>> foldDirectory
    (Map.toList
    >>> map snd
    >>> processFolder)
    (\i -> (0, i))
  >>> fst

processFolder :: [(Int, Int)] -> (Int, Int)
processFolder xs = (sum cs + if s < 100000 then s else 0, s)
  where
    (cs, ss) = unzip xs
    s = sum ss

main :: IO ()
main = compute <$> getContents >>= print