import Control.Arrow
import Data.List

data Direction = R | U | L | D
  deriving (Read, Show, Eq)

type Step = (Direction, Int)

readStep :: String -> Step
readStep (d:_:xs) = (read (d:[]), read xs)

type Position = (Int, Int)

delta :: Direction -> Position
delta R = ( 1, 0)
delta U = ( 0, 1)
delta L = (-1, 0)
delta D = ( 0,-1)

touch :: Position -> Position -> Bool
touch (x,y) (x',y') = abs (x-x') <= 1 && abs (y-y') <= 1

infixl 6 +#

(+#) :: Position -> Position -> Position
(x,y) +# (x',y') = (x+x',y+y')

(-#) :: Position -> Position -> Position
(x,y) -# (x',y') = (x-x',y-y')

signum' :: Position -> Position
signum' (x,y) = (signum x, signum y)

expand :: [Step] -> [Direction]
expand = map (replicate <$> snd <*> fst) >>> concat

moveHead :: [Direction] -> [Position]
moveHead = scanl (\p d -> p +# delta d) (0,0)

moveTail :: [Position] -> [Position]
moveTail = scanl1 (\t h -> if touch t h then t else t +# signum' (h -# t))

moveTails :: Int -> [Position] -> [Position]
moveTails 0 = id
moveTails n = moveTail >>> moveTails (n-1)

compute :: String -> Int
compute = lines
  >>> map readStep
  >>> expand
  >>> moveHead
  >>> moveTails 9
  >>> nub
  >>> length

main :: IO ()
main = (compute <$> getContents) >>= print