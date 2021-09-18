module Test where

import Quadtree

-- 1 0 0 0
-- 0 1 0 0
-- 0 0 1 0
-- 0 0 0 1
e = Branch (Branch (Leaf 1) (Leaf 0) (Leaf 0) (Leaf 1)) (Leaf 0) (Leaf 0) (Branch (Leaf 1) (Leaf 0) (Leaf 0) (Leaf 1)) :: QMatrix Int

-- 1 2 0 0
-- 3 0 0 0
-- 1 1 4 0
-- 1 1 0 5
m1 = Branch (Branch (Leaf 1) (Leaf 2) (Leaf 3) (Leaf 0)) (Leaf 0) (Leaf 1) (Branch (Leaf 4) (Leaf 0) (Leaf 0) (Leaf 5)) :: QMatrix Int

-- 1 8 0 0
-- 5 0 2 0
-- 0 0 0 4
-- 0 0 0 3
m2 = Branch (Branch (Leaf 1) (Leaf 8) (Leaf 5) (Leaf 0)) (Branch (Leaf 0) (Leaf 0) (Leaf 2) (Leaf 0)) (Leaf 0) (Branch (Leaf 0) (Leaf 4) (Leaf 0) (Leaf 3)) :: QMatrix Int

--  2 10  0  0
--  8  0  2  0
--  1  1  4  4
--  1  1  0  8
plus = Branch (Branch (Leaf 2) (Leaf 10) (Leaf 8) (Leaf 0)) (Branch (Leaf 0) (Leaf 0) (Leaf 2) (Leaf 0)) (Leaf 1) (Branch (Leaf 4) (Leaf 4) (Leaf 0) (Leaf 8)) :: QMatrix Int

-- 11  8  4  0
--  3 24  0  0
--  6  8  2 16
--  6  8  2 15
mult = Branch (Branch (Leaf 11) (Leaf 8) (Leaf 3) (Leaf 24)) (Branch (Leaf 4) (Leaf 0) (Leaf 0) (Leaf 0)) (Branch (Leaf 6) (Leaf 8) (Leaf 6) (Leaf 8)) (Branch (Leaf 2) (Leaf 16) (Leaf 2) (Leaf 15)) :: QMatrix Int

testPlus :: Bool
testPlus = m1 + m2 == plus

testMult :: Bool
testMult = m1 * m2 == mult

testIdentity :: Bool
testIdentity = (m1 * e == m1) && (m2 * e == m2)
