module GeneralAlgo where

import Grammar
import QMatrix
import Data.List (nub)

type Graph = QMatrix [NonTerm]

plusMatrix :: Eq c => QMatrix a -> QMatrix b -> (a -> b -> c) -> QMatrix c
plusMatrix (Leaf a k1) (Leaf b k2) plus
    | k1 == k2 = Leaf (plus a b) k1
    | otherwise = mismatchError
plusMatrix (Branch tl tr bl br) (Leaf b k) plus
    | k >= 2 = let e = Leaf b (k `div` 2) in fold_once $ Branch (plusMatrix tl e plus) (plusMatrix tr e plus) (plusMatrix bl e plus) (plusMatrix br e plus)
    | otherwise = mismatchError
plusMatrix (Leaf a k) (Branch tl tr bl br) plus
    | k >= 2 = let e = Leaf a (k `div` 2) in fold_once $ Branch (plusMatrix e tl plus) (plusMatrix e tr plus) (plusMatrix e bl plus) (plusMatrix e br plus)
    | otherwise = mismatchError
plusMatrix (Branch tl1 tr1 bl1 br1) (Branch tl2 tr2 bl2 br2) plus = fold_once $ Branch (plusMatrix tl1 tl2 plus) (plusMatrix tr1 tr2 plus) (plusMatrix bl1 bl2 plus) (plusMatrix br1 br2 plus)

multMatrix :: Eq c => QMatrix a -> QMatrix b -> (c -> c -> c) -> (a -> b -> c) -> QMatrix c
multMatrix (Leaf a k1) (Leaf b k2) plus mult
    | k1 == k2 = if k1 == 1 then Leaf (mult a b) 1 else let res = multMatrix (Leaf a (k1 `div` 2)) (Leaf b (k2 `div` 2)) plus mult in let e = plusMatrix res res plus in fold_once $ Branch e e e e
    | otherwise = mismatchError
multMatrix (Branch tl tr bl br) (Leaf b k) plus mult
    | k >= 2 = let e = Leaf b (k `div` 2) in let (x1, x2) = (plusMatrix (multMatrix tl e plus mult) (multMatrix tr e plus mult) plus, plusMatrix (multMatrix bl e plus mult) (multMatrix br e plus mult) plus) in fold_once $ Branch x1 x1 x2 x2
    | otherwise = mismatchError
multMatrix (Leaf a k) (Branch tl tr bl br) plus mult
    | k >= 2 = let e = Leaf a (k `div` 2) in let (x1, x2) = (plusMatrix (multMatrix e tl plus mult) (multMatrix e bl plus mult) plus, plusMatrix (multMatrix e tr plus mult) (multMatrix e br plus mult) plus) in fold_once $ Branch x1 x2 x1 x2
    | otherwise = mismatchError
multMatrix (Branch tl1 tr1 bl1 br1) (Branch tl2 tr2 bl2 br2) plus mult = fold_once $ Branch (plusMatrix (multMatrix tl1 tl2 plus mult) (multMatrix tr1 bl2 plus mult) plus) (plusMatrix (multMatrix tl1 tr2 plus mult) (multMatrix tr1 br2 plus mult) plus) (plusMatrix (multMatrix bl1 tl2 plus mult) (multMatrix br1 bl2 plus mult) plus) (plusMatrix (multMatrix bl1 tr2 plus mult) (multMatrix br1 br2 plus mult) plus)

plusNon :: [NonTerm] -> [NonTerm] -> [NonTerm]
plusNon non1 non2 = nub $ non1 ++ non2

multNon :: [Rule] -> [NonTerm] -> [NonTerm] -> [NonTerm]
multNon rules non1 non2 = [l | (Complex (l, (r1, r2))) <- rules, n1 <- non1, n2 <- non2, r1 == n1 && r2 == n2]

transform :: QMatrix [NonTerm] -> NonTerm -> QMatrix Bool
transform (Leaf a k) start = Leaf (elem start a) k
transform (Branch tl tr bl br) start = fold_once $ Branch (transform tl start) (transform tr start) (transform bl start) (transform br start)

solve :: Graph -> Grammar -> QMatrix Bool
solve graph grammar = transform (traclos graph) (start grammar) where
    traclos :: Graph -> Graph
    traclos x = let y = plusMatrix (multMatrix x x plusNon multOp) x plusNon in if y == x then y else traclos y where
        multOp = multNon (rules grammar)

one_step :: Graph -> Grammar -> Graph
one_step (Leaf n k) grammar = Leaf (nub $ [l | (Simple (l, x)) <- (rules grammar), y <- n, x == y] ++ [l | (Epsilon l) <- (rules grammar)]) k
one_step (Branch tl tr bl br) grammar = fold_once $ Branch (one_step tl grammar) (one_step tr grammar) (one_step bl grammar) (one_step br grammar)
