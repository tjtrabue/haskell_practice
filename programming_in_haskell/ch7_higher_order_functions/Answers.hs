module Ch7Answers where

-- #1 Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
-- using the higher-order functions map and filter.

-- Solution:
-- map_and_filter f p xs = filter p $ map f xs
-- that is, [f x | x <- xs, p x] === filter p $ map f xs

{-
#2 Without looking at the definitions from the standard prelude, define
the following higher-order library functions:
a. Decide if all elements of a list satisfy a predicate:
   all :: (a -> Bool) -> [a] -> Bool
b. Decide if any element of a list satisfies a predicate:
   any :: (a -> Bool) -> [a] -> Bool
c. Select elements from a list while they satisfy a predicate
   takeWhile :: (a -> Bool) -> [a] -> [a]
d. Remove elements from a list while they satisfy a predicate
   dropWhile :: (a -> Bool) -> [a] -> [a]
-}

-- Solution:
-- a.
myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldl (\b a -> b && p a) True
-- b.
myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldl (\b a -> b || p a) True
