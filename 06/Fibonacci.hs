{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

-- import Data.List

fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Ex. 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Ex. 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Ex. 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = foldr (\k l -> k ++ ", " ++ l) "..." .
         map show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons m ms) = m : streamToList ms

stream1 :: Stream Integer
stream1 = Cons 1 stream1

stream2 :: Stream Char
stream2 = Cons 's' stream2

stream3 :: Stream Integer
stream3 = Cons 4 (Cons 3 (Cons 2 stream1))

stream4 :: a -> (a -> a) -> Stream a
stream4 n f = Cons n (stream4 (f n) f)

-- import Data.Char
-- λ> stream4 'g' (chr . succ . ord)
-- λ> stream4 'C' (chr . pred . ord)

-- Ex. 4

streamRepeat :: a -> Stream a
streamRepeat m = Cons m (streamRepeat m)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons m ms) = Cons (f m) (streamMap f ms)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

-- Ex. 5

nats :: Stream Integer
nats = streamFromSeed succ 0

-- First version with divisibility tests
ruler0 :: Stream Integer
ruler0 = streamMap (\n -> rulerValue n (floor (log2 n))) $
         streamFromSeed succ 1

log2 :: Integer -> Float
log2 n = log (fromInteger n) / log 2

divides :: Integer -> Integer -> Bool
divides n m = m `mod` n == 0

rulerValue :: Integer -> Integer -> Integer
rulerValue n power = if (2^power) `divides` n
                     then power
                     else rulerValue n (pred power)

-- Second version (the "clever" one)
ruler :: Stream Integer
ruler = twistFrom 0 where
  twistFrom n = interleaveStreams (streamRepeat n) (twistFrom (n + 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons m ms) ns =
  Cons m (interleaveStreams ns ms)

-- Ex. 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate ss = streamMap negate ss
  (Cons u us) + (Cons v vs) = Cons (u + v) (us + vs)
  (Cons u us) * vv@(Cons v vs) =
    Cons (u * v) (streamMap (u *) vs + us * vv)

instance Fractional (Stream Integer) where
  (Cons u us) / (Cons v vs) = q where
    q = Cons (u `div` v) (streamMap (`div` v) (us - q * vs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2 :: Integer))

-- Some extra functions:

-- Produce the n-th Fibonacci number
fibn :: Int -> Integer
fibn n = head . drop n $ streamToList fibs3

-- Golden ratio
golden :: Double
golden = (fromInteger (fibn 501)) / (fromInteger (fibn 500))

-- Ex. 7

data Matrix = Matrix Row Row deriving Show
type Row = (Integer, Integer)

instance Num Matrix where
  fromInteger n = Matrix (n,0) (0,n)
  negate mx = fromInteger (-1) * mx
  Matrix (a,b) (c,d) + Matrix (e,f) (g,h) =
    Matrix (a+e, b+f) (c+g, d+h)
  Matrix (a,b) (c,d) * Matrix (e,f) (g,h) =
    Matrix (a*e + b*g, a*f + b*h) (c*e + d*g, c*f + d*h)

fib4 :: Integer -> Integer
fib4 n
  | n == 0 = 0
  | otherwise = project (bigF ^ n) where
      project (Matrix (_,p) _) = p
      bigF = Matrix (1,1) (1,0)

main :: IO ()
main = print $ fib4 10000

-- This number takes 200KB:
--writeFile "millionth_fib.txt" ((show (fib4 1000000)) ++ "\n")
