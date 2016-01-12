{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import Data.Char
import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map as M

-- Ex. 1

expr1 :: ExprT
expr1 = Mul (Add (Lit 2) (Lit 3)) (Lit 4)

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Mul e1 e2) = eval e1 * eval e2
eval (Add e1 e2) = eval e1 + eval e2

-- Ex. 2

{- Example parsings: -}
parse1, parse2, parse3 :: Maybe ExprT
parse1 = parseExp Lit Add Mul "(2+3)*4"
parse2 = parseExp Lit Add Mul "2+3*4"
parse3 = parseExp Lit Add Mul "2+3*"

evalStr :: String -> Maybe Integer
{- First version with explicit case analysis: -}
--evalStr ex = case (parseExp Lit Add Mul ex) of
--  Nothing -> Nothing
--  Just p  -> Just (eval p)
{- Version 2 with bind: -}
--evalStr ex = parseExp Lit Add Mul ex >>= (\p -> return (eval p))
{- Version 3 is the most compact of the three: -}
evalStr = fmap eval . parseExp Lit Add Mul

-- Ex. 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit i = (Lit i)
  add ex ey = (Add ex ey)
  mul ex ey = (Mul ex ey)

--mul (add (lit 2) (lit 3)) (lit 4) :: ExprT -- evaluates to ExprT
--eval $ mul (add (lit 2) (lit 3)) (lit 4) -- evaluates to 20

reify :: ExprT -> ExprT
reify = id

--reify $ mul (add (lit 2) (lit 3)) (lit 4) -- constrains the type to ExprT

-- Ex. 4

instance Expr Integer where
  lit i = i
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit i | i <= 0 = False
        | otherwise = True
  add x y = x || y
  mul x y = x && y

instance Expr MinMax where
  lit i = MinMax i
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
  lit i = Mod7 $ i `mod` 7
  --add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  --mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7   Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer; testInteger = testExp
testBool    :: Maybe Bool;    testBool    = testExp
testMM      :: Maybe MinMax;  testMM      = testExp
testSat     :: Maybe Mod7;    testSat     = testExp

-- Ex. 5

instance Expr S.Program where
  lit i = [S.PushI i]
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

-- To execute the compiled program and get a result in various formats:
--compile "-2+11*4" >>= return . stackVM
-- or
--stackVM $ maybe [] id $ compile "(12*4)+10"
-- or
--maybe (Left "Bad program.") stackVM $ compile "(-12*4)+(3*5)"
-- or
--(either (map toUpper) (\ (IVal v) -> show v) .
-- maybe (Left "Bad program.") stackVM . compile) "-1*(7+4)*2"

-- Ex. 6

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var name = Var name

instance Expr VarExprT where
  lit i = (VLit i)
  add ex ey = (VAdd ex ey)
  mul ex ey = (VMul ex ey)

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup 
         
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit i = \_ -> Just i
  add = op (+)
  mul = op (*)

op :: Monad m => (a -> b -> c) -> (t -> m a) -> (t -> m b) -> t -> m c 
op f ex ey = \d -> ex d >>= \x -> ey d >>= \y -> return (x `f` y)
-- 'd' as in dictionary

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

