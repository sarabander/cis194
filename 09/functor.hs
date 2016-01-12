{-# OPTIONS_GHC -Wall #-}

-- Some exercises from Typeclassopedia

--import Data.Char
import Prelude hiding (Functor, fmap)

-- 1.
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor (Either e) where
  fmap g (Right x) = Right (g x)
  fmap _ (Left x) = Left x

instance Functor ((->) e) where
  fmap = (.)

-- 2.
instance Functor ((,) e) where
  fmap g (x, y) = (x, g y)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)

-- Pair and (,) contain two values, but both values of Pair must have the
-- same type. Members of ordinary pairs can be different types.

-- 3.
data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
  fmap g (Leaf h) = Leaf (g . h)
  fmap g (Node itrees) = Node (map (fmap g) itrees)

-- 4.
-- Don't know.

-- 5.
-- True, but we have to use appropriate function to map over it, for example:
-- fmap (fmap (+7)) (Pair (Right 5) (Right 13)) ==
-- Pair (Right 12) (Right 20)
-- or
-- fmap (fmap (+7)) $ (((,) 6) . Right) 3 ==
-- (6,Right 10)


-- Second pair of exercises

-- 1.
data Ls a = Nil | Cns (Ls a) (Ls a) deriving (Show, Eq)

instance Functor Ls where
  fmap _ Nil = Nil
  fmap g (Cns _ xs) = Cns Nil (fmap g xs)

{-
> fmap id (Cns (Cns Nil Nil) Nil)
Cns Nil Nil

--- First law:
> fmap id (Cns (Cns Nil Nil) Nil) == id (Cns (Cns Nil Nil) Nil)
False

--- Second law:
> fmap (id . id) (Cns (Cns Nil Nil) Nil) ==
  (fmap id . fmap id) (Cns (Cns Nil Nil) Nil)
True
-}

-- 2.
-- Both laws are violated.
