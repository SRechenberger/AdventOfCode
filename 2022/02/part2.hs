import Control.Arrow
import Data.Tuple

data Hand = Rock | Paper | Scissors
  deriving (Eq, Show)

data Result = Lose | Draw | Win
  deriving (Eq, Show)

score :: Hand -> Int
score Rock     = 1
score Paper    = 2
score Scissors = 3

compileHand :: String -> Hand
compileHand "A" = Rock
compileHand "B" = Paper
compileHand "C" = Scissors

compileResult :: String -> Result
compileResult "X" = Lose
compileResult "Y" = Draw
compileResult "Z" = Win

calculateAnswer :: Result -> Hand -> Hand
calculateAnswer Lose Rock     = Scissors
calculateAnswer Lose Paper    = Rock
calculateAnswer Lose Scissors = Paper
calculateAnswer Win  Rock     = Paper
calculateAnswer Win  Paper    = Scissors
calculateAnswer Win  Scissors = Rock
calculateAnswer Draw a        = a

compileLine :: String -> (Result, Hand)
compileLine l = (compileResult r, compileHand a)
  where
    [a,r] = words l

evalLine :: Hand -> Hand -> Int
evalLine a        b | a == b = 3 + score b
evalLine Rock     Paper      = 6 + score Paper
evalLine Paper    Scissors   = 6 + score Scissors
evalLine Scissors Rock       = 6 + score Rock
evalLine _        x          = score x

compute :: String -> Int
compute = lines
  >>> map compileLine
  >>> map (\(r,s) -> (s, calculateAnswer r s))
  >>> map (uncurry evalLine)
  >>> sum

main :: IO ()
main = (compute <$> getContents) >>= print