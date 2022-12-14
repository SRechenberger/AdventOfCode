import Control.Arrow

data Hand = Rock | Paper | Scissors
  deriving (Eq, Show)

score :: Hand -> Int
score Rock     = 1
score Paper    = 2
score Scissors = 3

compile, compile' :: String -> Hand
compile "A" = Rock
compile "B" = Paper
compile "C" = Scissors

compile' "X" = Rock
compile' "Y" = Paper
compile' "Z" = Scissors

compileLine :: String -> (Hand, Hand)
compileLine l = (compile a, compile' r)
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
  >>> map (uncurry evalLine)
  >>> sum

main :: IO ()
main = (compute <$> getContents) >>= print