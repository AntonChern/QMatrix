module Main where

import Quadtree

-- 0 2 0 0
-- 0 0 0 0
-- 0 0 9 0
-- 0 0 0 0
matrix1 = Branch (Branch (Leaf 0) (Leaf 2) (Leaf 0) (Leaf 0)) (Leaf 0) (Leaf 0) (Branch (Leaf 9) (Leaf 0) (Leaf 0) (Leaf 0)) :: QMatrix Int

-- 0 0 0 0
-- 0 0 0 7
-- 0 4 0 0
-- 3 0 0 0
matrix2 = Branch (Leaf 0) (Branch (Leaf 0) (Leaf 0) (Leaf 0) (Leaf 7)) (Branch (Leaf 0) (Leaf 4) (Leaf 3) (Leaf 0)) (Leaf 0) :: QMatrix Int

main :: IO ()
main = do
    putStrLn (show matrix1)
    putStrLn "*"
    putStrLn (show matrix2)
    putStrLn "="
    --  0  0  0 14
    --  0  0  0  0
    --  0 36  0  0
    --  0  0  0  0
    putStrLn (show $ matrix1 * matrix2)
