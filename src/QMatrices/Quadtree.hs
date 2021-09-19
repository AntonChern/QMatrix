module Quadtree where

data QMatrix a = Leaf a | Branch (QMatrix a) (QMatrix a) (QMatrix a) (QMatrix a)
    deriving (Eq, Show)

fold :: Eq a => QMatrix a -> QMatrix a
fold (Leaf a) = Leaf a
fold (Branch tl tr bl br) = fold' (Branch (fold tl) (fold tr ) (fold bl) (fold br))
    where
        fold' :: Eq a => QMatrix a -> QMatrix a
        fold' (Branch a@(Leaf tl) b@(Leaf tr) c@(Leaf bl) d@(Leaf br))
            | tl == tr && tr == bl && bl == br = a
            | otherwise = Branch a b c d
        fold' p = p

instance (Num a, Eq a) => Num (QMatrix a) where
    (Leaf a) + (Leaf b) = Leaf (a + b)
    (Branch tl tr bl br) + (Leaf a) = fold $ Branch (tl + Leaf a) (tr + Leaf a) (bl + Leaf a) (br + Leaf a)
    (Leaf a) + (Branch tl tr bl br) = fold $ Branch (Leaf a + tl) (Leaf a + tr) (Leaf a + bl) (Leaf a + br)
    (Branch tl1 tr1 bl1 br1) + (Branch tl2 tr2 bl2 br2) = Branch (tl1 + tl2) (tr1 + tr2) (bl1 + bl2) (br1 + br2)
    (Leaf a) * (Leaf b) = Leaf (a * b)
    (Branch tl tr bl br) * (Leaf a) = fold $ Branch (tl * Leaf a + tr * Leaf a) (tl * Leaf a + tr * Leaf a) (bl * Leaf a + br * Leaf a) (bl * Leaf a + br * Leaf a)
    (Leaf a) * (Branch tl tr bl br) = fold $ Branch (Leaf a * tl + Leaf a * bl) (Leaf a * tr + Leaf a * br) (Leaf a * tl + Leaf a * bl) (Leaf a * tr + Leaf a * br)
    (Branch tl1 tr1 bl1 br1) * (Branch tl2 tr2 bl2 br2) = fold $ Branch (tl1 * tl2 + tr1 * bl2) (tl1 * tr2 + tr1 * br2) (bl1 * tl2 + br1 * bl2) (bl1 * tr2 + br1 * br2)
