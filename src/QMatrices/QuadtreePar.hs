module QuadtreePar where

import Data.Semiring
import Control.DeepSeq
import Control.Monad.Par hiding (NFData)
-- import Control.Concurrent

data QMatrix a = Leaf a Int | Branch (QMatrix a) (QMatrix a) (QMatrix a) (QMatrix a)
    deriving (Eq, Show)

instance NFData a => NFData (QMatrix a) where
--     rnf m = seq m ()
    rnf (Leaf a k) = rnf a `seq` rnf k
    rnf (Branch tl tr bl br) = seq (rnf tl) (seq (rnf tr) (seq (rnf bl) (rnf br)))
    
-- rnf :: QMatrix a -> ()
-- rnf (Leaf a k) = a `seq` k `seq` ()
-- rnf (Branch tl tr bl br) = rnf tl `seq` rnf tr `seq` rnf bl `seq` rnf br `seq` ()
--     deepseq a b = rnf a `seq` b
    
-- fold' :: Eq a => QMatrix a -> QMatrix a
-- fold' (Branch a@(Leaf tl k1) b@(Leaf tr k2) c@(Leaf bl k3) d@(Leaf br k4))
--     | tl == tr && tr == bl && bl == br = Leaf tl (2 * k1)
--     | otherwise = Branch a b c d
-- fold' p = p

-- fold :: Eq a => QMatrix a -> QMatrix a
-- fold (Branch tl tr bl br) = let
--                                 x1 = runPar $ fork $ 
--                             in runPar $ fold' $ Branch x1 x2 x3 x4



-- fold' :: Eq a => QMatrix a -> QMatrix a
-- fold' (Branch a@(Leaf tl k1) b@(Leaf tr k2) c@(Leaf bl k3) d@(Leaf br k4))
--     | tl == tr && tr == bl && bl == br = Leaf tl (2 * k1)
--     | otherwise = Branch a b c d
-- fold' p = p
-- bfs :: Eq a => [(IVar (QMatrix a), QMatrix a)] -> Par ()
-- bfs [] = return ()
-- bfs list = bfs $ concatMap f list
-- f :: Eq a => (IVar (QMatrix a), QMatrix a) -> [(IVar (QMatrix a), QMatrix a)]
-- f (i, l@(Leaf a k)) = runPar $ do
--     fork $ put_ i l
--     return []
-- f (i, (Branch tl tr bl br)) = runPar $ do
--     j1 <- new
--     j2 <- new
--     j3 <- new
--     j4 <- new
--     fork $ do x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4; put_ i (fold' $ Branch x1 x2 x3 x4)
--     return [(j1, tl), (j2, tr), (j3, bl), (j4, br)]
-- fold :: Eq a => QMatrix a -> QMatrix a
-- fold m = runPar $ do
--     i <- new
--     bfs [(i, m)]
--     get i


fold''' :: Eq a => QMatrix a -> QMatrix a
fold''' (Branch a@(Leaf tl k1) b@(Leaf tr k2) c@(Leaf bl k3) d@(Leaf br k4))
    | tl == tr && tr == bl && bl == br = Leaf tl (2 * k1)
    | otherwise = Branch a b c d
fold''' p = p

fold :: (Eq a, NFData a) => QMatrix a -> QMatrix a
fold m = let
             fold'' :: (Eq a, NFData a) => QMatrix a -> QMatrix a
             fold'' (Branch a@(Leaf tl k1) b@(Leaf tr k2) c@(Leaf bl k3) d@(Leaf br k4))
                 | tl == tr && tr == bl && bl == br = Leaf tl (2 * k1)
                 | otherwise = Branch a b c d
             fold'' p = p
         in let
                fold' :: (Eq a, NFData a) => QMatrix a -> IVar (QMatrix a) -> Par ()
                fold' l@(Leaf a k) i = put i l
                fold' (Branch tl tr bl br) i = do
                    j1 <- new
                    j2 <- new
                    j3 <- new
                    j4 <- new
                    fork $ fold' tl j1
                    fork $ fold' tr j2
                    fork $ fold' bl j3
                    fork $ fold' br j4
                    x1 <- get j1
                    x2 <- get j2
                    x3 <- get j3
                    x4 <- get j4
                    put i (fold'' $ Branch x1 x2 x3 x4)
            in runPar $ do
                i <- new
                fork $ fold' m i
                get i

    
-- fold :: Eq a => QMatrix a -> QMatrix a
-- fold (Leaf a k) = Leaf a k
-- fold (Branch tl tr bl br) = runPar $ do
--     i1 <- new
--     i2 <- new
--     i3 <- new
--     i4 <- new
--     fork $ put_ i1 (fold tl)
--     fork $ put_ i2 (fold tr)
--     fork $ put_ i3 (fold bl)
--     fork $ put_ i4 (fold br)
--     f_tl <- get i1
--     f_tr <- get i2
--     f_bl <- get i3
--     f_br <- get i4
--     return (fold' $ Branch f_tl f_tr f_bl f_br)
--     calc_tl <- (spawn_ . return) (fold tl)
--     calc_tr <- (spawn_ . return) (fold tr)
--     calc_bl <- (spawn_ . return) (fold bl)
--     calc_br <- (spawn_ . return) (fold br)
--     new_tl <- get calc_tl
--     new_tr <- get calc_tr
--     new_bl <- get calc_bl
--     new_br <- get calc_br
--     return (fold' $ Branch new_tl new_tr new_bl new_br)
            
-- fromList :: Eq a => [[a]] -> QMatrix a
-- fromList [[a]] = Leaf a 1
-- fromList list = let
--                     rows = length (head list)
--                     cols = length list
--                 in let
--                        left :: [[a]] -> [[a]]
--                        left = (map (take (cols `div` 2)))
--                        right :: [[a]] -> [[a]]
--                        right = (map (reverse . (take (cols `div` 2)) . reverse))
--                        top :: [[a]] -> [[a]]
--                        top = (take (rows `div` 2))
--                        bottom :: [[a]] -> [[a]]
--                        bottom = ((reverse . (take (rows `div` 2)) . reverse))
--                    in let
--                           tl = top (left list)
--                           tr = top (right list)
--                           bl = bottom (left list)
--                           br = bottom (right list)
--                       in runPar $ do
--                           calc_tl <- new
--                           calc_tr <- new
--                           calc_bl <- new
--                           calc_br <- new
--                           fork (put_ calc_tl (fromList tl))
--                           fork (put_ calc_tr (fromList tr))
--                           fork (put_ calc_bl (fromList bl))
--                           fork (put_ calc_br (fromList br))
--                           new_tl <- get calc_tl
--                           new_tr <- get calc_tr
--                           new_bl <- get calc_bl
--                           new_br <- get calc_br
--                           return (fold $ Branch new_tl new_tr new_bl new_br)

size :: QMatrix a -> Int
size (Leaf _ k) = k
size (Branch tl tr bl br) = 2 * size tl
                          
qinsert :: Eq a => Int -> Int -> a -> QMatrix a -> QMatrix a
qinsert i j v (Leaf a k)
    | k == 1 = if i == 1 && j == 1 then (Leaf v k) else (Leaf a k)
    | i <= k `div` 2 && j <= k `div` 2 = fold''' $ Branch (qinsert i j v (Leaf a (k `div` 2))) (Leaf a (k `div` 2)) (Leaf a (k `div` 2)) (Leaf a (k `div` 2))
    | i <= k `div` 2 && j > k `div` 2 = fold''' $ Branch (Leaf a (k `div` 2)) (qinsert i (j - k `div` 2) v (Leaf a (k `div` 2))) (Leaf a (k `div` 2)) (Leaf a (k `div` 2))
    | i > k `div` 2 && j <= k `div` 2 = fold''' $ Branch (Leaf a (k `div` 2)) (Leaf a (k `div` 2)) (qinsert (i - k `div` 2) j v (Leaf a (k `div` 2))) (Leaf a (k `div` 2))
    | i > k `div` 2 && j > k `div` 2 = fold''' $ Branch (Leaf a (k `div` 2)) (Leaf a (k `div` 2)) (Leaf a (k `div` 2)) (qinsert (i - k `div` 2) (j - k `div` 2) v (Leaf a (k `div` 2)))
    | otherwise = Leaf a k
qinsert i j v m@(Branch tl tr bl br)
    | i >= (size m) || j >= (size m) = m
    | i <= (size m) `div` 2 && j <= (size m) `div` 2 = fold''' $ Branch (qinsert i j v tl) tr bl br
    | i <= (size m) `div` 2 && j > (size m) `div` 2 = fold''' $ Branch tl (qinsert i (j - (size m) `div` 2) v tr) bl br
    | i > (size m) `div` 2 && j <= (size m) `div` 2 = fold''' $ Branch tl tr (qinsert (i - (size m) `div` 2) j v bl) br
    | i > (size m) `div` 2 && j > (size m) `div` 2 = fold''' $ Branch tl tr bl (qinsert (i - (size m) `div` 2) (j - (size m) `div` 2) v br)

assign :: Int -> Int
assign 1 = 1
assign k = 2 * (assign $ k `div` 2)
    
fromList' :: (Eq a, Semiring a, NFData a) => [((Int, Int), a)] -> QMatrix a
fromList' list = let
                     sizeOfList = maximum $ concatMap (\((a, b), _) -> [a, b]) list
                     sizeOfMatrix = assign sizeOfList
                 in foldr (\((i, j), v) m -> qinsert i j v m) (Leaf zero (sizeOfMatrix)) list
        
mismatchError = error "Mismatch of sizes"

f :: Int -> Int
f 0 = 1
f 1 = 2
f 2 = 3
f x = f (x - 3) + x + f (x - 2)



app :: (Eq a, Semiring a, NFData a) => QMatrix a -> QMatrix a -> IVar (QMatrix a) -> Par ()
app (Leaf a k1) (Leaf b k2) i
    | k1 == k2 = put i (Leaf (a <+> b) k1)
    | otherwise = mismatchError
app (Branch tl tr bl br) (Leaf a k) i
    | k >= 2 = let
                nl = Leaf a (k `div` 2)
            in do
        j1 <- new
        j2 <- new
        j3 <- new
        j4 <- new
        fork $ app tl nl j1
        fork $ app tr nl j2
        fork $ app bl nl j3
        fork $ app br nl j4
        x1 <- get j1
        x2 <- get j2
        x3 <- get j3
        x4 <- get j4
        put i (Branch x1 x2 x3 x4)
--         fork $ do x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4; put i (Branch x1 x2 x3 x4)
    | otherwise = mismatchError
app (Leaf a k) (Branch tl tr bl br) i
    | k >= 2 = let
                nl = Leaf a (k `div` 2)
            in do
        j1 <- new
        j2 <- new
        j3 <- new
        j4 <- new
        fork $ app nl tl j1
        fork $ app nl tr j2
        fork $ app nl bl j3
        fork $ app nl br j4
        x1 <- get j1
        x2 <- get j2
        x3 <- get j3
        x4 <- get j4
        put i (Branch x1 x2 x3 x4)
--         fork $ do x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4; put i (Branch x1 x2 x3 x4)
    | otherwise = mismatchError
app (Branch tl1 tr1 bl1 br1) (Branch tl2 tr2 bl2 br2) i = do
    j1 <- new
    j2 <- new
    j3 <- new
    j4 <- new
    fork $ app tl1 tl2 j1
    fork $ app tr1 tr2 j2
    fork $ app bl1 bl2 j3
    fork $ app br1 br2 j4
    x1 <- get j1
    x2 <- get j2
    x3 <- get j3
    x4 <- get j4
    put i (fold''' $ Branch x1 x2 x3 x4)
--     fork $ do x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4; put i (fold''' $ Branch x1 x2 x3 x4)


instance (Eq a, Semiring a, NFData a) => Num (QMatrix a) where
--     (Leaf a k1) + (Leaf b k2)
--         | k1 == k2 = runPar $ do
--             i <- new
--             fork $ put i (f 65)
--             x <- get i
--             return $ Leaf (a <+> b) x
--         | k1 == k2 = Leaf (a <+> b) k1--(f 65)
--         | k1 == k2 = runPar $ do
--             i <- new
--             fork $ put i (a <+> b)
--             x <- get i
--             return $ Leaf x k1
--         | k1 == k2 = runPar $ do
--             i0 <- new
--             i <- new
--             fork $ put_ i (a <+> b)
--             fork $ do x1 <- get i; put_ i0 (Leaf x1 k1)
--             get i0
--         | otherwise = mismatchError
--     (Branch tl tr bl br) + (Leaf a k)
--         | k >= 2 = runPar $ do
--             j1 <- new
--             j2 <- new
--             j3 <- new
--             j4 <- new
--             fork $ put j1 (tl + Leaf a (k `div` 2))
--             fork $ put j2 (tr + Leaf a (k `div` 2))
--             fork $ put j3 (bl + Leaf a (k `div` 2))
--             fork $ put j4 (br + Leaf a (k `div` 2))
--             x1 <- get j1
--             x2 <- get j2
--             x3 <- get j3
--             x4 <- get j4
--             return $ Branch x1 x2 x3 x4
--         | otherwise = mismatchError
--     (Leaf a k) + (Branch tl tr bl br)
--         | k >= 2 = runPar $ do
--             j1 <- new
--             j2 <- new
--             j3 <- new
--             j4 <- new
--             fork $ put j1 (Leaf a (k `div` 2) + tl)
--             fork $ put j2 (Leaf a (k `div` 2) + tr)
--             fork $ put j3 (Leaf a (k `div` 2) + bl)
--             fork $ put j4 (Leaf a (k `div` 2) + br)
--             x1 <- get j1
--             x2 <- get j2
--             x3 <- get j3
--             x4 <- get j4
--             return $ Branch x1 x2 x3 x4
--         | otherwise = mismatchError

    m1 + m2 = runPar $ do
        i <- new
        fork $ app m1 m2 i
        get i
--     (Branch tl1 tr1 bl1 br1) + (Branch tl2 tr2 bl2 br2) = runPar $ do
--         j1 <- new
--         j2 <- new
--         j3 <- new
--         j4 <- new
--         fork $ put j1 (tl1 + tl2)
--         fork $ put j2 (tr1 + tr2)
--         fork $ put j3 (bl1 + bl2)
--         fork $ put j4 (br1 + br2)
--         x1 <- get j1
--         x2 <- get j2
--         x3 <- get j3
--         x4 <- get j4
--         return $ Branch x1 x2 x3 x4
        
--     (Branch tl tr bl br) + (Leaf a k)
--         | k >= 2 = runPar $ do
--             calc_tl <- (spawn_ . return) (tl + Leaf a (k `div` 2))
--             calc_tr <- (spawn_ . return) (tr + Leaf a (k `div` 2))
--             calc_bl <- (spawn_ . return) (bl + Leaf a (k `div` 2))
--             calc_br <- (spawn_ . return) (br + Leaf a (k `div` 2))
--             new_tl <- get calc_tl
--             new_tr <- get calc_tr
--             new_bl <- get calc_bl
--             new_br <- get calc_br
--             return (fold $ Branch new_tl new_tr new_bl new_br)
--         | otherwise = mismatchError
--     (Leaf a k) + (Branch tl tr bl br)
--         | k >= 2 = runPar $ do
--             calc_tl <- (spawn_ . return) (Leaf a (k `div` 2) + tl)
--             calc_tr <- (spawn_ . return) (Leaf a (k `div` 2) + tr)
--             calc_bl <- (spawn_ . return) (Leaf a (k `div` 2) + bl)
--             calc_br <- (spawn_ . return) (Leaf a (k `div` 2) + br)
--             new_tl <- get calc_tl
--             new_tr <- get calc_tr
--             new_bl <- get calc_bl
--             new_br <- get calc_br
--             return (fold $ Branch new_tl new_tr new_bl new_br)
--         | otherwise = mismatchError
--     (Branch tl1 tr1 bl1 br1) + (Branch tl2 tr2 bl2 br2) = runPar $ do
--         calc_tl <- (spawn_ . return) (tl1 + tl2)
--         calc_tr <- (spawn_ . return) (tr1 + tr2)
--         calc_bl <- (spawn_ . return) (bl1 + bl2)
--         calc_br <- (spawn_ . return) (br1 + br2)
--         tl <- get calc_tl
--         tr <- get calc_tr
--         bl <- get calc_bl
--         br <- get calc_br
--         return (fold $ Branch tl tr bl br)

--     (Leaf a k1) + (Leaf b k2)
--         | k1 == k2 = Leaf (a <+> b) k1
--         | otherwise = mismatchError
--     (Branch tl tr bl br) + (Leaf a k)
--         | k >= 2 = fold $ Branch (tl + Leaf a (k `div` 2)) (tr + Leaf a (k `div` 2)) (bl + Leaf a (k `div` 2)) (br + Leaf a (k `div` 2))
--         | otherwise = mismatchError
--     (Leaf a k) + (Branch tl tr bl br)
--         | k >= 2 = fold $ Branch (Leaf a (k `div` 2) + tl) (Leaf a (k `div` 2) + tr) (Leaf a (k `div` 2) + bl) (Leaf a (k `div` 2) + br)
--         | otherwise = mismatchError
--     (Branch tl1 tr1 bl1 br1) + (Branch tl2 tr2 bl2 br2) = Branch (tl1 + tl2) (tr1 + tr2) (bl1 + bl2) (br1 + br2)

    m1 * m2 = let
                    multiply :: (Eq a, Semiring a, NFData a) => QMatrix a -> QMatrix a -> IVar (QMatrix a) -> Par ()
                    multiply (Leaf a k1) (Leaf b k2) i
                        | k1 == k2 = let
                                        mult :: Semiring a => Int -> a -> a
                                        mult 1 a = a
                                        mult k a = let prev = mult (k `div` 2) a
                                                    in prev <+> prev
                                    in put i (Leaf (mult k1 (a <.> b)) k1)
                        | otherwise = mismatchError
                    multiply (Branch tl tr bl br) (Leaf a k) i
                        | k >= 2 = let
                                    nl = Leaf a (k `div` 2)
                                in do
                            n1 <- new
                            n2 <- new
                            n3 <- new
                            n4 <- new
                            j1 <- new
                            j2 <- new
                            fork $ multiply tl nl n1
                            fork $ multiply tr nl n2
                            fork $ multiply bl nl n3
                            fork $ multiply br nl n4
--                             fork $ do v1 <- get n1; v2 <- get n2; put j1 (v1 + v2)
--                             fork $ do v1 <- get n3; v2 <- get n4; put j2 (v1 + v2)
                            fork $ do v1 <- get n1; v2 <- get n2; app v1 v2 j1
                            fork $ do v1 <- get n3; v2 <- get n4; app v1 v2 j2
                            fork $ do x1 <- get j1; x2 <- get j2; put i (fold''' $ Branch x1 x1 x2 x2)
--                             x1 <- get j1
--                             x2 <- get j2
--                             x3 <- get j3
--                             x4 <- get j4
--                             put i (Branch x1 x2 x3 x4)
                        | otherwise = mismatchError
                    multiply (Leaf a k) (Branch tl tr bl br) i
                        | k >= 2 = let
                                    nl = Leaf a (k `div` 2)
                                in do
                            n1 <- new
                            n2 <- new
                            n3 <- new
                            n4 <- new
                            j1 <- new
                            j2 <- new
                            fork $ multiply nl tl n1
                            fork $ multiply nl tr n2
                            fork $ multiply nl bl n3
                            fork $ multiply nl br n4
--                             fork $ do v1 <- get n1; v2 <- get n3; put j1 (v1 + v2)
--                             fork $ do v1 <- get n2; v2 <- get n4; put j2 (v1 + v2)
                            fork $ do v1 <- get n1; v2 <- get n3; app v1 v2 j1
                            fork $ do v1 <- get n2; v2 <- get n4; app v1 v2 j2
                            fork $ do x1 <- get j1; x2 <- get j2; put i (fold''' $ Branch x1 x2 x1 x2)
--                             x1 <- get j1
--                             x2 <- get j2
--                             x3 <- get j3
--                             x4 <- get j4
--                             put i (Branch x1 x2 x3 x4)
                        | otherwise = mismatchError
                    multiply b1@(Branch tl1 tr1 bl1 br1) b2@(Branch tl2 tr2 bl2 br2) i = do
                        n11 <- new
                        n12 <- new
                        j1 <- new
                        n21 <- new
                        n22 <- new
                        j2 <- new
                        n31 <- new
                        n32 <- new
                        j3 <- new
                        n41 <- new
                        n42 <- new
                        j4 <- new
                        fork $ multiply tl1 tl2 n11
                        fork $ multiply tr1 bl2 n12
                        fork $ do v1 <- get n11; v2 <- get n12; app v1 v2 j1
--                         fork $ do v1 <- get n11; v2 <- get n12; put j1 (v1 + v2)
                        fork $ multiply tl1 tr2 n21
                        fork $ multiply tr1 br2 n22
--                         fork $ do v1 <- get n21; v2 <- get n22; put j2 (v1 + v2)
                        fork $ do v1 <- get n21; v2 <- get n22; app v1 v2 j2
                        fork $ multiply bl1 tl2 n31
                        fork $ multiply br1 bl2 n32
--                         fork $ do v1 <- get n31; v2 <- get n32; put j3 (v1 + v2)
                        fork $ do v1 <- get n31; v2 <- get n32; app v1 v2 j3
                        fork $ multiply bl1 tr2 n41
                        fork $ multiply br1 br2 n42
--                         fork $ do v1 <- get n41; v2 <- get n42; put j4 (v1 + v2)
                        fork $ do v1 <- get n41; v2 <- get n42; app v1 v2 j4
                        fork $ do x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4; put i (fold''' $ Branch x1 x2 x3 x4)
--                         x1 <- get j1
--                         x2 <- get j2
--                         x3 <- get j3
--                         x4 <- get j4
--                         put i (Branch x1 x2 x3 x4)
              in runPar $ do
        i <- new
        fork $ multiply m1 m2 i
        get i
--     (Leaf a k1) * (Leaf b k2)
--         | k1 == k2 = let
--                          mult :: Semiring a => Int -> a -> a
--                          mult 1 a = a
--                          mult k a = let prev = mult (k `div` 2) a
--                                     in prev <+> prev
--                      in Leaf (mult k1 (a <.> b)) k1
--         | otherwise = mismatchError
--     (Branch tl tr bl br) * (Leaf a k)
--         | k >= 2 = runPar $ do
--             calc_tl <- (spawn_ . return) (tl * Leaf a (k `div` 2) + tr * Leaf a (k `div` 2))
--             calc_tr <- (spawn_ . return) (tl * Leaf a (k `div` 2) + tr * Leaf a (k `div` 2))
--             calc_bl <- (spawn_ . return) (bl * Leaf a (k `div` 2) + br * Leaf a (k `div` 2))
--             calc_br <- (spawn_ . return) (bl * Leaf a (k `div` 2) + br * Leaf a (k `div` 2))
--             new_tl <- get calc_tl
--             new_tr <- get calc_tr
--             new_bl <- get calc_bl
--             new_br <- get calc_br
--             return (fold $ Branch new_tl new_tr new_bl new_br)
--         | otherwise = mismatchError
--     (Leaf a k) * (Branch tl tr bl br)
--         | k >= 2 = runPar $ do
--             calc_tl <- (spawn_ . return) (Leaf a (k `div` 2) * tl + Leaf a (k `div` 2) * bl)
--             calc_tr <- (spawn_ . return) (Leaf a (k `div` 2) * tr + Leaf a (k `div` 2) * br)
--             calc_bl <- (spawn_ . return) (Leaf a (k `div` 2) * tl + Leaf a (k `div` 2) * bl)
--             calc_br <- (spawn_ . return) (Leaf a (k `div` 2) * tr + Leaf a (k `div` 2) * br)
--             new_tl <- get calc_tl
--             new_tr <- get calc_tr
--             new_bl <- get calc_bl
--             new_br <- get calc_br
--             return (fold $ Branch new_tl new_tr new_bl new_br)
--         | otherwise = mismatchError
--     (Branch tl1 tr1 bl1 br1) * (Branch tl2 tr2 bl2 br2) = runPar $ do
--             calc_tl <- (spawn_ . return) (tl1 * tl2 + tr1 * bl2)
--             calc_tr <- (spawn_ . return) (tl1 * tr2 + tr1 * br2)
--             calc_bl <- (spawn_ . return) (bl1 * tl2 + br1 * bl2)
--             calc_br <- (spawn_ . return) (bl1 * tr2 + br1 * br2)
--             tl <- get calc_tl
--             tr <- get calc_tr
--             bl <- get calc_bl
--             br <- get calc_br
--             return (fold $ Branch tl tr bl br)
