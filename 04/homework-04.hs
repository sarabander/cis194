{-# OPTIONS_GHC -Wall #-}

import Test.QuickCheck
import Data.List
import Data.Numbers.Primes

-- Ex. 1

-- 1.
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- is better expressed as

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

prop_fun1 :: [Integer] -> Bool
prop_fun1 xs = fun1 xs == fun1' xs

main :: IO ()
main = quickCheck prop_fun1

-- 2.
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- is better expressed as

fun2' :: Integer -> Integer
fun2' = foldr (+) 0 . filter even . takeWhile (>1) .
        iterate (\n -> if even n then n `div` 2 else 3*n + 1)

prop_fun2 :: Integer -> Bool
prop_fun2 n = fun2 n == fun2' n

test_prop_fun2 :: Integer -> Bool
test_prop_fun2 = and . map prop_fun2 . enumFromTo 1

{- Don't test prop_fun2 with 'quickCheck', test it with 'test_prop_fun2 n',
   where n < 10000 takes reasonable time. -}

-- Ex. 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

insertNode :: a -> Tree a -> Tree a
insertNode newNode Leaf = Node 0 Leaf newNode Leaf
insertNode newNode (Node _ oldLeft node oldRight) =
  let (newLeft, newRight) =
        if height oldLeft < height oldRight
        then (insertNode newNode oldLeft, oldRight)
        else (oldLeft, insertNode newNode oldRight) in
   let newHeight = 1 + max (height newLeft) (height newRight) in
    Node newHeight newLeft node newRight

height :: Tree a -> Integer
height Leaf = (-1)
height (Node h _ _ _) = h

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ leftTree _ rightTree) =
  isBalanced leftTree && isBalanced rightTree &&
  abs (height leftTree - height rightTree) <= 1

-- Ex. 3

-- 1.
binaryXor :: Bool -> Bool -> Bool
binaryXor True False = True
binaryXor False True = True
binaryXor _ _ = False

xor :: [Bool] -> Bool
xor = foldr binaryXor False

-- 2.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

-- 3.
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (.) id (map (flip f) (reverse xs)) $ base

-- Ex. 4

seq1 :: Integer -> [Integer]
{- The sequence to be excluded from the [1..n] list later.
   1 <= i <= j and i + j + 2ij <= n. -}
seq1 n = filter (<= n) $ [i + j + 2*i*j | j <- [1..n], i <- [1..j]]

seq2 :: Integer -> [Integer]
{- Alternative generator of the same sequence. -}
seq2 n = filter (<= n) $ map (\(i,j) -> i+j+2*i*j) $
         filter (\(x, y) -> x <= y) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
{- Cartesian product. -}
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
{- The sieve that generates odd primes less than 2n + 2. -}
sieveSundaram n = (map ((+1) . (2*) . head) . filter (\x -> length x == 1) .
                   group . sort . ([1..n] ++) . seq1) n

{- We concatenate [1..n] list with 'seq1 n' and sort the result. Now there
   are duplicate integers in the list if the integer is found in seq1.
   'group' will produce two-element sublists out of these duplicates.
   We keep only the sublists with a single element -- now all the integers
   that were in seq1 are eliminated from [1..n]. The result is flattened,
   doubled and incremented. -}

primeCheck :: Integer -> Bool
{- Checks if all the numbers produced by sieveSundaram are primes. -}
primeCheck = all isPrime . sieveSundaram
