-- | Answers to exervices from Chapter 8 of Programming in Haskell.

module Answers where

-- A natural number type
data Nat = Zero | Succ Nat

-- Turn a natural number into an Int
evalNat :: Nat -> Int
evalNat Zero     = 0
evalNat (Succ n) = 1 + evalNat n

-- Add two natural numbers, producing another
addNat :: Nat -> Nat -> Nat
addNat Zero n     = n
addNat (Succ m) n = Succ (addNat m n)

n :: Nat
n = Succ(Succ(Succ(Succ Zero)))

m :: Nat
m = Succ(Succ(Succ Zero))

-- NOTE: You cannot put a type constraint on a data type declaration in Haskell.
--       We must declare a Tree with an Ord parameter type, such as Int.
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

{-
#1 In a similar manner to the function add,a define a recursive multiplication
function mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
Hint: make use of add in your definition.
-}
mult :: Nat -> Nat -> Nat
mult _ Zero        = Zero
mult m (Succ Zero) = m
mult m (Succ n)    = addNat m (mult m n)

{-
#2
-}
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
-- occurs x (Node l y r) = x == y || occurs x l || occurs x r
occurs x (Node l y r) = case compare x y of
  EQ -> True
  LT -> occurs x l
  GT -> occurs x r

-- This is more efficient than the old version of occurs because this one only
-- considers a single path down the tree, rather than all paths. However,
-- this implementation assumes that the tree is a search tree.

{-
#3
-}

{-
#4
-}

{-
#5
-}

{-
#6
-}

{-
#7
-}

{-
#8
-}

{-
#9
-}
