-- | Answers to exervices from Chapter 8 of Programming in Haskell.

module Answers where

-- A natural number type
import           Data.List
data Nat = Zero | Succ Nat

-- Turn a natural number into an Int
evalNat :: Nat -> Int
evalNat Zero     = 0
evalNat (Succ n) = 1 + evalNat n

-- Add two natural numbers, producing another
addNat :: Nat -> Nat -> Nat
addNat Zero     n = n
addNat (Succ m) n = Succ (addNat m n)

n :: Nat
n = Succ (Succ (Succ (Succ Zero)))

m :: Nat
m = Succ (Succ (Succ Zero))

-- NOTE: You cannot put a type constraint on a data type declaration in Haskell.
--       We must declare a Tree with an Ord parameter type, such as Int.
data Tree a = Leaf a | Node (Maybe (Tree a)) a (Maybe (Tree a))
  deriving (Show)

t :: Tree Int
-- t = Node ((Node (Leaf 1)) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
t = Node (Just (Node (Just (Leaf 1)) 3 (Just (Leaf 4))))
         5
         (Just (Node (Just (Leaf 6)) 7 (Just (Leaf 9))))

data Expr = Val Int | Add Expr Expr

{-
#1 In a similar manner to the function add,a define a recursive multiplication
function mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
Hint: make use of add in your definition.
-}
mult :: Nat -> Nat -> Nat
mult _ Zero        = Zero
mult m (Succ Zero) = m
mult m (Succ n   ) = addNat m (mult m n)

{-
#2
-}
occurs :: Ord a => a -> Maybe (Tree a) -> Bool
occurs _ Nothing                       = False
occurs x (Just (Leaf y              )) = x == y
occurs x (Just (Node l y r)) = case compare x y of
  EQ -> True
  LT -> occurs x l
  GT -> occurs x r

-- This is more efficient than the old version of occurs because this one only
-- considers a single path down the tree, rather than all paths. However,
-- this implementation assumes that the tree is a search tree.

{-
#3
-}
descendAndCountLeaves :: Maybe (Tree a) -> Integer
descendAndCountLeaves Nothing             = 0
descendAndCountLeaves (Just (Leaf _    )) = 1
descendAndCountLeaves (Just (Node l _ r)) =
  descendAndCountLeaves l + descendAndCountLeaves r

numLeaves :: Tree a -> Integer
numLeaves x = descendAndCountLeaves (Just x)

balanced :: Tree a -> Bool
balanced (Leaf _    ) = True
balanced (Node l _ r) =
  abs (descendAndCountLeaves l - descendAndCountLeaves r) <= 1

{-
#4
-}

splitList :: [a] -> ([a], [a])
splitList xs = splitAt (length xs `div` 2) xs

balance :: Ord a => [a] -> Maybe (Tree a)
balance []  = Nothing
balance [x] = Just (Leaf x)
balance xs  = Just (Node (balance first) m (balance second))
  where (first, (m : second)) = splitList . sort $ xs

{-
#5
-}
-- We can think of f as a transformer function on each value of the expression,
-- and g as a binary operator for the transformed values.
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val i)     = f i
folde f g (Add e1 e2) = folde f g e1 `g` folde f g e2

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
