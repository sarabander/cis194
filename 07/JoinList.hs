{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Data.Monoid
import Sized
import Test.QuickCheck
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- Ex. 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag (Single t _) = t
tag (Append t _ _) = t
tag Empty = error "Empty JoinList has no tag."                    

-- Ex. 2

-- 1.
-- Helper function to get plain integer size from JoinList
tagSize :: (Sized m, Monoid m) => JoinList m a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing -- unindexable emptiness
indexJ i _ | i < 0 = Nothing
indexJ i node | i >= tagSize node = Nothing
indexJ _ (Single _ v) = Just v
indexJ i (Append _ l r)
  | i < tagSize l = indexJ i l
  | otherwise = indexJ (i - tagSize l) r

pred_indexJ :: Int -> Bool
pred_indexJ i = (indexJ i testList1) == (jlToList testList1 !!? i)

-- Helper functions from lecture notes
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- 2.
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n <= 0 = jl
           | n >= tagSize jl = Empty
dropJ _ single@(Single _ _) = single
dropJ n (Append _ l r)
  | n < tagSize l = (dropJ n l) +++ r
  | otherwise = dropJ (n - tagSize l) r

pred_dropJ :: Int -> Bool
pred_dropJ n = jlToList (dropJ n testList1) == drop n (jlToList testList1)

testList1 :: JoinList Size String
testList1 = (Append (Size 4)
             (Append (Size 2)
              (Single (Size 1) "moon")
              (Single (Size 1) "planet"))
             (Append (Size 2)
              (Single (Size 1) "star")
              (Single (Size 1) "galaxy")))

-- 3.
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl | n <= 0 = Empty
           | n >= tagSize jl = jl
takeJ _ single@(Single _ _) = single
takeJ n (Append _ l r)
  | n < tagSize l = takeJ n l
  | otherwise = l +++ takeJ (n - tagSize l) r

pred_takeJ :: Int -> Bool
pred_takeJ n = jlToList (takeJ n testList1) == take n (jlToList testList1)

test :: IO ()
test = putStr "Testing takeJ:  " >> quickCheck pred_takeJ >>
       putStr "Testing dropJ:  " >> quickCheck pred_dropJ >>
       putStr "Testing indexJ: " >> quickCheck pred_indexJ

-- Ex. 3

-- See also module 'Scrabble'.

scoreLine :: String -> JoinList Score String
scoreLine string = Single annotation string where
  annotation = scoreString string

-- Ex. 4

scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine string = Single annotation string where
  annotation = (scoreString string, Size 1)

-- Helper function to get plain integer score from JoinList
tagScore :: JoinList (Score, Size) String -> Int
tagScore = getScore . fst . tag

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList

  fromString = fromLines . lines where
    fromLines [] = Empty
    fromLines (l:[]) = scoreSizeLine l
    fromLines ls = fromLines (take half ls) +++
                   fromLines (drop half ls) where
                     half = length ls `div` 2
  line = indexJ

  replaceLine _ _ Empty = Empty 
  replaceLine n _ buf | n < 0 || n >= tagSize buf = buf
  replaceLine n ln buf =
    takeJ n buf +++ scoreSizeLine ln +++ dropJ (n + 1) buf

  numLines = tagSize
  value = tagScore

initBuf :: JoinList (Score, Size) String 
initBuf = fromString $ unlines
         [ "This buffer is for poems you want to discard,"
         , "and for tinkering with steam valve coefficients."
         , "To load a different file, type the character 'l'"
         , "followed by the name of the file."
         ]
            
main :: IO ()
main = runEditor editor $ initBuf
