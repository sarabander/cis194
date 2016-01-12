{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List
-- import Test.QuickCheck

-- Ex. 1

-- 1.
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) =
  GL (emp : emps) (empFun emp + fun)

-- 2.
instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL emps1 fun1) (GL emps2 fun2) =
    GL (emps1 <> emps2) (fun1 + fun2)

prop_assoc :: GuestList -> GuestList -> GuestList -> Bool
prop_assoc gl1 gl2 gl3 = (gl1 <> gl2) <> gl3 == gl1 <> (gl2 <> gl3)

--test1 = quickCheck prop_assoc

-- 3.
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Ex. 2

treeFold :: (a -> b -> b) -> b -> Tree a -> b  -- right fold
treeFold f acc (Node root forest) =
  f root (foldr (flip (treeFold f)) acc forest)

-- For example:
--treeFold glCons (GL [] 0) testCompany 

-- Ex. 3

nextLevel :: Employee -> [(GuestList, GuestList)]
             -> (GuestList, GuestList)
nextLevel emp gls = (glCons emp secondBest, firstBest) where
  firstBest  = mconcat . map fst $ gls
  secondBest = mconcat . map snd $ gls
             
-- Ex. 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . maxFunPair where
  maxFunPair (Node emp []) = (glCons emp mempty, mempty)
  maxFunPair (Node emp subtrees) =
    nextLevel emp $ map maxFunPair subtrees

-- Ex. 5

totalFun :: GuestList -> Fun
totalFun (GL _ fun) = fun

guestNames :: GuestList -> [Name]
guestNames (GL emps _) = sort $ map empName emps

-- Rather ugly way to cons a header, which is not a name.
pickGuests :: Tree Employee -> [Name]
pickGuests empTree = header : guestNames guests where
  guests = maxFun empTree
  header = "Total fun: " ++ (show $ totalFun guests)

main :: IO ()
main = readFile "company.txt" >>=
       mapM_ putStrLn . pickGuests . read
