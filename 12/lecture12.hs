-- How to throw dice based on lecture notes

import Control.Monad.Random

die :: (RandomGen g) => Rand g Int
die = getRandomR (1,6)

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence (replicate n die)

main = do
  values <- evalRandIO (dice 2)
  putStrLn (show values)

