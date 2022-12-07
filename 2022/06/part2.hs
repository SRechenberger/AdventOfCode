import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List


compute :: String -> Int
compute xs@(_:xs')
  | length (nub (take 14 xs)) == 14 = 14
  | otherwise = 1 + compute xs'

main :: IO ()
main = (lines >>> map compute) <$> getContents >>= mapM_ print