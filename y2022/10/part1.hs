module Main where
  
import Control.Arrow

data Command = NoOp | Addx Int | AddWait
  deriving (Show, Eq)

readCommand :: String -> Command
readCommand "noop" = NoOp
readCommand addx   = Addx . read . tail . dropWhile (/=' ') $ addx

cycles :: Command -> Int
cycles NoOp     = 1
cycles (Addx _) = 2

expand :: [Command] -> [Command]
expand = map e >>> concat
  where
    e NoOp     = [NoOp]
    e (Addx i) = [AddWait, Addx i]

eval :: (Int, Int) -> Command -> (Int, Int)
eval (i, c) (Addx d) = (i + d, c+1)
eval (i, c) _        = (i,     c+1)

compute :: String -> Int
compute = lines
  >>> map readCommand
  >>> expand
  >>> scanl eval (1,1)
  >>> filter (snd >>> (`elem` [20,60..220]))
  >>> map (uncurry (*))
  >>> sum

main :: IO ()
main = compute <$> getContents >>= print