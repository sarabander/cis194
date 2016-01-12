{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List

-- Ex. 1
-- =====

skips :: [a] -> [[a]]
{- Given a list of length n, produces a list of lists, where the first list
   has all the elements of the original list, the second has every second
   element, third every third, and the last one has the n-th element. -}
skips xs = map (map (xs !!)) picks where
  picks = [[m, 2*m + 1 .. end] | m <- [0 .. end]] where
    end = length xs - 1

{- It works by first generating a list of picks, where every pick is a list
   containing the indices of the original list we want to retain. Here's how:

   'end' is the last index of 'xs', 'm' will get values 0,1..end. 'm' is
   then used to generate the sequence [m, 2*m + 1 .. end] for each pick.

   When m = 0, the first pick is [0,1..end], when m = 1, the second pick is
   [1,3..end], when m = 2, the third pick is [2,5..end], etc.

   We can see that first pick selects every element, second pick selects
   every second element, third pick selects every third element, etc.

   There is as many picks as there are elements in the original list.
   The list of picks is then mapped over by 'map (map (xs !!))'. The inner
   'map' maps over a pick and returns those elements of xs indicated by
   the numbers in the pick. -}

-- Examples, evaluate with 'skips sk1', etc.
sk1, sk2 :: String
sk1 = "ABCD"
sk2 = "hello!"

sk3 :: [Int]
sk3 = [1]

sk4, sk5 :: [Bool]
sk4 = [True,False]
sk5 = []

-- Ex. 2
-- =====

localMaxima :: [Integer] -> [Integer]
{- Finds the elements that are greater than the left and right neighbor. -}
localMaxima (l:c:r:rs)
  | l < c && c > r = c : localMaxima (r:rs)
  | otherwise = localMaxima (c:r:rs)
localMaxima _ = []

{- There are no local maxima in shorter lists than with three elements.
   If we have a list containing at least three elements, we begin scanning
   at the start, naming the first three elements 'l', 'c', and 'r' as in
   'left', 'center', and 'right'. If the center element is greater than
   both the neighbors, we have found the first local maximum.

   We continue searching local maxima from the remaining list that starts
   with the right neighbor 'r' of the last found maximum. Why not start
   with 'c' instead of 'r'? Because 'r' can't be the next local maximum,
   it's left neighbor already was the maximum, so 'r' is less than 'c'.

   If 'c' was not the local maximum, we continue by testing if it's right
   neighbor is. When the remaining list shrinks below length 3, we are
   finished, because now the last clause matches, giving the empty list. -}

-- Examples, evaluate with 'localMaxima lm1', etc.
lm1, lm2, lm3 :: [Integer]
lm1 = [2,9,5,6,1]
lm2 = [2,3,4,1,5]
lm3 = [1..5]

-- Ex. 3
-- =====

histogram :: [Integer] -> String
{- Graphs how many of each number (0..9) were there in the list. -}
histogram = foldr1 (++) . map (++ "\n") . addScale . stars where
  addScale = (++ [replicate 10 '=' ++ "\n" ++ ['0'..'9']])
  stars = (\cs -> [map (?>= m) cs | m <- reverse [1..maximum cs]]) . counts
    where n ?>= m = if n >= m then '*' else ' '

counts :: [Integer] -> [Int]
{- Counts the number of appearances of the digits 0..9 in the list.
   Returns a ten-element list: first element indicates how many zeroes
   there were, second shows the number of ones, etc., tenth element shows
   the number of nines. -}
counts = map (pred . length) . group . takeWhile (< 10) .
         dropWhile (< 0) . sort . ([0..9] ++)

{- The function 'counts' first prepends the list [0..9] to the integer list
   we are studying. This is to ensure that all ten digits (0..9) are
   represented in the list.

   Next step is sorting, after which the elements less than zero and greater
   than nine will be dropped (using functions 'dropWhile' and 'takeWhile').

   With 'group', all duplicate digits are grouped into sublists. The length
   of each sublist is found and the effect of injecting the list [0..9] into
   the original list is undone by decrementing the list lengths by one.

   'histogram' uses the 'counts' function to get the digit counts, finds the
   maximum count, enumerates from maximum to one, giving this value
   iteratively to m. For each m, 'map' transforms the list of counts to a
   string containing '*' for those digit count values that are at least m.
   Other digit positions get space characters. This is decided by (?>=).

   The result is a list of strings to which a bar of ten '=' and a scale of
   digits 0..9 will be appended by 'addScale'. To each string in the list a
   newline character gets appended and the whole list is concatenated to a
   single string by 'foldr1'. -}

-- Examples, evaluate with 'putStr $ histogram nrs1', etc.
nrs1, nrs2, nrs3  :: [Integer]
nrs1 = [1,1,1,5]
nrs2 = [1,4,5,4,6,6,3,4,2,4,9]
nrs3 = [9,6,7,0,(-7),1,19,43,6,0,3,9,5,1,6,0,(-1),9,2,20,8,6,7]
