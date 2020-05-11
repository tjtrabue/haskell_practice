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
-- c.
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p (x : xs) | null xs || not (p x) = []
                       | otherwise            = x : myTakeWhile p xs
-- d.
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p (x : xs) | null xs   = []
                       | not (p x) = xs
                       | otherwise = myDropWhile p xs

{-
#3 Redefine the functions map f and filter p using foldr.
-}

-- Solution:
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a bs -> f a : bs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr addIfPred []
 where
  addIfPred y ys | p y       = y : ys
                 | otherwise = ys

{-
#4 Using foldl, define a function dec2int :: [Int] -> Int that converts a
decimal number into an integer. For example,
> dec2int [2,3,4,5]
2345
-}

-- Solution:
dec2int :: [Int] -> Int
dec2int = foldl concatInt 0
  where concatInt x m = read (show x ++ show m) :: Int

{-
#5 Without looking at the definitions from the standard prelude, define the
higher-order library function curry that converts a function on pairs into
a curried function, and, conversely, the function uncurry that converts a
curried function with two arguments into a function on pairs.
-}
