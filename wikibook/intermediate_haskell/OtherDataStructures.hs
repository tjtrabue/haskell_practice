module OtherDataStructures (
) where

-- A basic tree data structure
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch x y) = Branch (treeMap f x) (treeMap f y)

main :: IO ()
main = do
  let x = Leaf 5
  let y = Leaf 1
  let t = Branch x y

  putStrLn "Original tree:"
  print t
  putStrLn "Transformed tree:"
  print $ treeMap (+ 5) t