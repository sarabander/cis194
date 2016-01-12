{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

-- Ex. 1
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Ex. 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = reverse (jumpOver (reverse ns))
  where jumpOver [] = []
        jumpOver (x:[]) = [x]
        jumpOver (x:y:zs) = x : 2*y : jumpOver zs

-- Ex. 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Ex. 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
-- Alternative solution that uses the composition operator:
--validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- Ex. 5
type Height = Int
type Peg = String
type Move = (Peg, Peg)

hanoi0 :: Height -> Peg -> Peg -> Peg -> [Move]
hanoi0 n a b c
  | n > 0 = hanoi0 (n - 1) a c b ++ [(a, b)] ++ hanoi0 (n - 1) c b a
  | otherwise = []

printList :: Show a => [a] -> IO ()
printList [] = putStr ""
printList (move:moves) =
  do putStr (show move ++ " ")
     _ <- getLine
     printList moves

-- Ex. 6
-- Just some constants to hold peg identifiers
start, spare1, spare2, end :: Peg
start = "a"
spare1 = "s1"
spare2 = "s2"
end = "b"

split :: Int -> (Int, Int)
split n =
  let half = n `div` 2
  in (n - half, half)

{-
hanoi2 :: Height -> Peg -> Peg -> Peg -> Peg -> Moves
hanoi2 n a b s1 s2 -- a: start, b: end, s1, s2: spares
  | n > 1 =
      let (k, l) = split (n - 1)
      in    hanoi2 k  a  s1 s2 b
         ++ hanoi2 l  a  s2 b  s1
         ++ [(a, b)]
         ++ hanoi2 l  s2 b  a  s1 
         ++ hanoi2 k  s1 b  s2 a
  | n == 1 = [(a, b)]
  | otherwise = []
-}

hanoi2 :: Height -> Peg -> Peg -> Peg -> Peg -> Moves
hanoi2 n a b s1 s2 -- a: start, b: end, s1, s2: spares
  | n > 1 =    hanoi2 (n - 2) a s1 s2 b  
            ++ [(a, s2)] ++ [(a, b)] ++ [(s2, b)]
            ++ hanoi2 (n - 2) s1 b a s2 
  | n == 1 = [(a, b)]
  | otherwise = []

data Hanoi = Hanoi { startPeg  :: Tower
                   , sparePeg1 :: Tower
                   , sparePeg2 :: Tower
                   , endPeg    :: Tower
                   } deriving Eq

instance Show Hanoi where
  show (Hanoi a s1 s2 b) =
    let e = "  " in  -- 'e' as in "empty space"
     "Hanoi " ++ show a ++ e ++ show s1 ++ e ++ show s2 ++ e ++ show b

type Tower = [Int]  -- a stack of disks, number represents size
type Moves = [Move] -- a list of moves solving the puzzle

data Solution = Solution Height Moves deriving (Eq, Show)

-- Solves the four-pegged Towers of Hanoi puzzle
solveHanoi4 :: Hanoi -> Solution
solveHanoi4 hanoi
  |validateStartConf hanoi = let height = findHeight hanoi in
     Solution height (hanoi2 height start end spare1 spare2)
  |otherwise = error "Invalid starting configuration."
               
startConf :: Hanoi
startConf = Hanoi { startPeg = [1..15]
                  , sparePeg1 = []
                  , sparePeg2 = []
                  , endPeg = []
                  }

invalidConf :: Hanoi
invalidConf = Hanoi [5, 3] [2,1] [4] [6]

-- Checks if we have a valid starting configuration:
-- starting peg is empty or is stacked with a conic tower;
-- other pegs are empty
validateStartConf :: Hanoi -> Bool
validateStartConf (Hanoi { startPeg, sparePeg1, sparePeg2, endPeg }) =
  conic startPeg && null (sparePeg1 ++ sparePeg2 ++ endPeg)

-- Checks if all pegs have conic stacks
validateConf :: Hanoi -> Bool
validateConf (Hanoi { startPeg, sparePeg1, sparePeg2, endPeg }) =
  and $ map conic [startPeg, sparePeg1, sparePeg2, endPeg]
  
-- Finds the height of starting stack
findHeight :: Hanoi -> Height
findHeight hanoi = length $ startPeg hanoi

-- Determines if tower is properly stacked: smaller disk on top of bigger
conic :: Tower -> Bool
conic tower = and $ zipWith (<) tower (tail tower)

-- Generates all the configurations from given starting configuration
generateConfs :: Hanoi -> [Hanoi]
generateConfs begin =
  let (Solution _ moves) = solveHanoi4 begin in
   reverse $ makeConfs [begin] moves where
     makeConfs confs [] = confs
     makeConfs confs@(conf:_) (move:moves) =
       makeConfs (nextConf move conf : confs) moves
     makeConfs [] (_ : _) = error "Starting conf not given."

-- Given a move and a configuration of towers, returns a new configuration
nextConf :: Move -> Hanoi -> Hanoi
nextConf move conf = 
  let (sourcePeg, destinationPeg) = move
      takeFromHanoi peg hanoi 
         |peg == start = let tower = startPeg hanoi in
          (head tower, hanoi {startPeg = tail tower})
         |peg == spare1 = let tower = sparePeg1 hanoi in
          (head tower, hanoi {sparePeg1 = tail tower})
         |peg == spare2 = let tower = sparePeg2 hanoi in 
          (head tower, hanoi {sparePeg2 = tail tower})
         |peg == end = let tower = endPeg hanoi in
          (head tower, hanoi {endPeg = tail tower})
         |otherwise = error "No such peg."
      putToHanoi peg disk hanoi
         |peg == start = let tower = startPeg hanoi in
          hanoi {startPeg = disk : tower}
         |peg == spare1 = let tower = sparePeg1 hanoi in
          hanoi {sparePeg1 = disk : tower}
         |peg == spare2 = let tower = sparePeg2 hanoi in
          hanoi {sparePeg2 = disk : tower}
         |peg == end = let tower = endPeg hanoi in
          hanoi {endPeg = disk : tower}
         |otherwise = error "No such peg."
  in let (disk, reducedHanoi) = takeFromHanoi sourcePeg conf 
     in putToHanoi destinationPeg disk reducedHanoi

-- Tests if all configurations are legal
legalConfs :: [Hanoi] -> Bool
legalConfs = and . map validateConf

testUptoN :: Int -> Bool
testUptoN n =
  let startConfs = [ Hanoi [1..m] [] [] [] | m <- [1..n] ]
  in and $ map (legalConfs . generateConfs) startConfs
