import Control.Arrow

data Command = NoOp | Addx Int | AddWait
  deriving (Show, Eq)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks i xs
  | i <= length xs = take i xs : chunks i (drop i xs)
  | otherwise      = [xs]

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

renderPixel :: (Int, Int) -> Char
renderPixel (i, c) = if abs (i - mod c 40) <= 1 then '#' else ' '

compute :: String -> [String]
compute = lines
  >>> map readCommand
  >>> expand
  >>> scanl eval (1,0)
  >>> map renderPixel
  >>> chunks 40

main :: IO ()
main = compute <$> getContents >>= mapM_ print