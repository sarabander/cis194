import Data.Monoid
import qualified Data.Foldable as F

-- From Learnyou
-- (http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids)
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) <>  
                    (vowels x `compare` vowels y) <>  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l <>
                           f x           <>
                           F.foldMap f r

testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty))  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty))

-- From https://wiki.haskell.org/Fold
map' f = foldr ((:) . f) []

-- For demonstrating the folding process:
foldRight = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
foldLeft  = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])

foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)
 
foldi            :: (a -> a -> a) -> a -> [a] -> a
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))
 
pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t

foldT = foldt (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
foldI = foldi (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])

-- Exercise about boolean monoids from Haskell Wikibook

newtype And = And Bool deriving (Show, Eq)
newtype Or  = Or  Bool deriving (Show, Eq)

instance Monoid And where
  mempty = And True
  And x `mappend` And y = And (x && y)

instance Monoid Or where
  mempty = Or False
  Or x `mappend` Or y = Or (x || y)

