module Answers
  (
  ) where

-- Problem #7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

 -- Problem #8
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort first_half) (msort second_half)
  where
    half_len = length xs `div` 2
    halve :: [b] -> ([b], [b])
    halve = splitAt half_len
    (first_half, second_half) = halve xs
