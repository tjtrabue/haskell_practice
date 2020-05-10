module Ch7Answers where

-- #1 Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
-- using the higher-order functions map and filter.

-- Solution:
-- map_and_filter f p xs = filter p $ map f xs
-- that is, [f x | x <- xs, p x] === filter p $ map f xs
