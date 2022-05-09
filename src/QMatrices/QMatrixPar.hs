module QMatrixPar where

import Data.Semiring
import Control.DeepSeq
import Control.Monad.Par hiding (NFData)
import GHC.Float

data QMatrix a = Leaf a Int | Branch (QMatrix a) (QMatrix a) (QMatrix a) (QMatrix a)
    deriving (Eq)--, Show)

instance Show a => Show (QMatrix a) where
    show m = "\n" ++ show' res (maxLen res) where
        res = toList m
show' :: [[String]] -> Int -> String
show' [x] k = showCool x k ++ "\n"
show' (x:xs) k = showCool x k ++ "\n" ++ (show' xs k)
toList :: Show a => QMatrix a -> [[String]]
toList (Leaf a k) = replicate k (replicate k (show a))
toList (Branch tl tr bl br) = (zipWith (++) (toList tl) (toList tr)) ++ (zipWith (++) (toList bl) (toList br))
showCool :: [String] -> Int -> String
showCool [x] _ = x
showCool (x:xs) k = x ++ (replicate (k - length x) ' ') ++ "|" ++ (showCool xs k)
maxLen :: [[String]] -> Int
maxLen list = maximum $ map maxLen' list where
    maxLen' :: [String] -> Int
    maxLen' list = maximum $ map length list
    
instance NFData a => NFData (QMatrix a) where
    rnf (Leaf a k) = rnf a `seq` rnf k
    rnf (Branch tl tr bl br) = seq (rnf tl) (seq (rnf tr) (seq (rnf bl) (rnf br)))

-- size -> sizeOfQM

sizeOfQM :: QMatrix a -> Int
sizeOfQM (Leaf _ k) = k
sizeOfQM (Branch tl tr bl br) = 2 * sizeOfQM tl

-- qinsert -> insertToQM

insertToQM :: Eq a => Int -> Int -> a -> QMatrix a -> QMatrix a
insertToQM i j v (Leaf a k)
    | k == 1 = if i == 1 && j == 1 then (Leaf v k) else (Leaf a k)
    | i <= k `div` 2 && j <= k `div` 2 = fold_once $ Branch (insertToQM i j v (Leaf a (k `div` 2))) (Leaf a (k `div` 2)) (Leaf a (k `div` 2)) (Leaf a (k `div` 2))
    | i <= k `div` 2 && j > k `div` 2 = fold_once $ Branch (Leaf a (k `div` 2)) (insertToQM i (j - k `div` 2) v (Leaf a (k `div` 2))) (Leaf a (k `div` 2)) (Leaf a (k `div` 2))
    | i > k `div` 2 && j <= k `div` 2 = fold_once $ Branch (Leaf a (k `div` 2)) (Leaf a (k `div` 2)) (insertToQM (i - k `div` 2) j v (Leaf a (k `div` 2))) (Leaf a (k `div` 2))
    | i > k `div` 2 && j > k `div` 2 = fold_once $ Branch (Leaf a (k `div` 2)) (Leaf a (k `div` 2)) (Leaf a (k `div` 2)) (insertToQM (i - k `div` 2) (j - k `div` 2) v (Leaf a (k `div` 2)))
    | otherwise = Leaf a k
insertToQM i j v m@(Branch tl tr bl br)
    | i > (sizeOfQM m) || j > (sizeOfQM m) = m
    | i <= (sizeOfQM m) `div` 2 && j <= (sizeOfQM m) `div` 2 = fold_once $ Branch (insertToQM i j v tl) tr bl br
    | i <= (sizeOfQM m) `div` 2 && j > (sizeOfQM m) `div` 2 = fold_once $ Branch tl (insertToQM i (j - (sizeOfQM m) `div` 2) v tr) bl br
    | i > (sizeOfQM m) `div` 2 && j <= (sizeOfQM m) `div` 2 = fold_once $ Branch tl tr (insertToQM (i - (sizeOfQM m) `div` 2) j v bl) br
    | i > (sizeOfQM m) `div` 2 && j > (sizeOfQM m) `div` 2 = fold_once $ Branch tl tr bl (insertToQM (i - (sizeOfQM m) `div` 2) (j - (sizeOfQM m) `div` 2) v br)

fold_once :: Eq a => QMatrix a -> QMatrix a
fold_once (Branch a@(Leaf tl k1) b@(Leaf tr k2) c@(Leaf bl k3) d@(Leaf br k4))
    | tl == tr && tr == bl && bl == br = Leaf tl (2 * k1)
    | otherwise = Branch a b c d
fold_once p = p

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
                    j1 <- new; j2 <- new; j3 <- new; j4 <- new
                    fork $ fold' tl j1
                    fork $ fold' tr j2
                    fork $ fold' bl j3
                    fork $ fold' br j4
                    x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4
                    put i (fold'' $ Branch x1 x2 x3 x4)
            in runPar $ do
                i <- new
                fork $ fold' m i
                get i

mismatchError = error "Mismatch of sizes"

app :: (Eq a, Semiring a, NFData a) => QMatrix a -> QMatrix a -> IVar (QMatrix a) -> Par ()
app (Leaf a k1) (Leaf b k2) i
    | k1 == k2 = put i (Leaf (a <+> b) k1)
    | otherwise = mismatchError
app (Branch tl tr bl br) (Leaf a k) i
    | k >= 2 = let
                nl = Leaf a (k `div` 2)
            in do
        j1 <- new; j2 <- new; j3 <- new; j4 <- new
        fork $ app tl nl j1
        fork $ app tr nl j2
        fork $ app bl nl j3
        fork $ app br nl j4
        x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4
        put i (fold_once $ Branch x1 x2 x3 x4)
    | otherwise = mismatchError
app (Leaf a k) (Branch tl tr bl br) i
    | k >= 2 = let
                nl = Leaf a (k `div` 2)
            in do
        j1 <- new; j2 <- new; j3 <- new; j4 <- new
        fork $ app nl tl j1
        fork $ app nl tr j2
        fork $ app nl bl j3
        fork $ app nl br j4
        x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4
        put i (fold_once $ Branch x1 x2 x3 x4)
    | otherwise = mismatchError
app (Branch tl1 tr1 bl1 br1) (Branch tl2 tr2 bl2 br2) i = do
    j1 <- new; j2 <- new; j3 <- new; j4 <- new
    fork $ app tl1 tl2 j1
    fork $ app tr1 tr2 j2
    fork $ app bl1 bl2 j3
    fork $ app br1 br2 j4
    x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4
    put i (fold_once $ Branch x1 x2 x3 x4)

instance (Eq a, Semiring a, NFData a) => Num (QMatrix a) where
    m1 + m2 = runPar $ do
        i <- new
        fork $ app m1 m2 i
        get i
    m1 * m2 = let
                    multiply :: (Eq a, Semiring a, NFData a) => QMatrix a -> QMatrix a -> IVar (QMatrix a) -> Par ()
                    multiply (Leaf a k1) (Leaf b k2) i
                        | k1 == k2 = if k1 == 1 then put i (Leaf (a <.> b) 1) else do
                            n <- new; j <- new
                            fork $ multiply (Leaf a (k1 `div` 2)) (Leaf b (k2 `div` 2)) n
                            fork $ do v <- get n; app v v j
                            fork $ do x <- get j; put i (fold_once $ Branch x x x x)
                        | otherwise = mismatchError
                    multiply (Branch tl tr bl br) (Leaf a k) i
                        | k >= 2 = let
                                    nl = Leaf a (k `div` 2)
                                in do
                            n1 <- new; n2 <- new; n3 <- new; n4 <- new; j1 <- new; j2 <- new
                            fork $ multiply tl nl n1
                            fork $ multiply tr nl n2
                            fork $ multiply bl nl n3
                            fork $ multiply br nl n4
                            fork $ do v1 <- get n1; v2 <- get n2; app v1 v2 j1
                            fork $ do v1 <- get n3; v2 <- get n4; app v1 v2 j2
                            fork $ do x1 <- get j1; x2 <- get j2; put i (fold_once $ Branch x1 x1 x2 x2)
                        | otherwise = mismatchError
                    multiply (Leaf a k) (Branch tl tr bl br) i
                        | k >= 2 = let
                                    nl = Leaf a (k `div` 2)
                                in do
                            n1 <- new; n2 <- new; n3 <- new; n4 <- new; j1 <- new; j2 <- new
                            fork $ multiply nl tl n1
                            fork $ multiply nl tr n2
                            fork $ multiply nl bl n3
                            fork $ multiply nl br n4
                            fork $ do v1 <- get n1; v2 <- get n3; app v1 v2 j1
                            fork $ do v1 <- get n2; v2 <- get n4; app v1 v2 j2
                            fork $ do x1 <- get j1; x2 <- get j2; put i (fold_once $ Branch x1 x2 x1 x2)
                        | otherwise = mismatchError
                    multiply b1@(Branch tl1 tr1 bl1 br1) b2@(Branch tl2 tr2 bl2 br2) i = do
                        n11 <- new; n12 <- new; j1 <- new; n21 <- new; n22 <- new; j2 <- new; n31 <- new; n32 <- new; j3 <- new; n41 <- new; n42 <- new; j4 <- new
                        fork $ multiply tl1 tl2 n11
                        fork $ multiply tr1 bl2 n12
                        fork $ do v1 <- get n11; v2 <- get n12; app v1 v2 j1
                        fork $ multiply tl1 tr2 n21
                        fork $ multiply tr1 br2 n22
                        fork $ do v1 <- get n21; v2 <- get n22; app v1 v2 j2
                        fork $ multiply bl1 tl2 n31
                        fork $ multiply br1 bl2 n32
                        fork $ do v1 <- get n31; v2 <- get n32; app v1 v2 j3
                        fork $ multiply bl1 tr2 n41
                        fork $ multiply br1 br2 n42
                        fork $ do v1 <- get n41; v2 <- get n42; app v1 v2 j4
                        fork $ do x1 <- get j1; x2 <- get j2; x3 <- get j3; x4 <- get j4; put i (fold_once $ Branch x1 x2 x3 x4)
              in runPar $ do
        i <- new
        fork $ multiply m1 m2 i
        get i

{-Transformation-}

assignLower :: Int -> Int
assignLower 1 = 1
assignLower k = 2 * (assignLower $ k `div` 2)

assignUpper :: Int -> Int
assignUpper 1 = 1
assignUpper x = 2 ^ (truncate $ logBase 2 (2 * ((int2Float x) - 1)))

listToQMWith :: Eq a => [((Int, Int), a)] -> a -> QMatrix a
listToQMWith list zero_elem = let nodes = 1 + (maximum $ concatMap (\((a, b), _) -> [a, b]) list)
                                  sizeOfMatrix = assignUpper nodes
                              in foldr (\((i, j), v) m -> insertToQM (i + 1) (j + 1) v m) (Leaf zero_elem sizeOfMatrix) list

listToQM :: (Semiring a, Eq a) => [((Int, Int), a)] -> QMatrix a
listToQM list = let nodes = 1 + (maximum $ concatMap (\((a, b), _) -> [a, b]) list)
                    sizeOfMatrix = assignUpper nodes
                in foldr (\((i, j), v) m -> insertToQM (i + 1) (j + 1) v m) (Leaf zero sizeOfMatrix) list

fullListToQMWith :: Eq a => [[a]] -> a -> QMatrix a
fullListToQMWith [[a]] _ = Leaf a 1
fullListToQMWith list zero_elem = let sizeOfMatrix = assignUpper (length list)
                                      sizeOfQuarter = sizeOfMatrix `div` 2
                                      new_list = (map (\x -> x ++ replicate (sizeOfMatrix - length list) zero_elem) list) ++ replicate (sizeOfMatrix - length list) (replicate sizeOfMatrix zero_elem)
                                      (t, b) = splitAt sizeOfQuarter new_list
                                      (tl, tr) = unzip $ map (\x -> splitAt sizeOfQuarter x) t
                                      (bl, br) = unzip $ map (\x -> splitAt sizeOfQuarter x) b
                                  in fold_once $ Branch (fullListToQMCut tl) (fullListToQMCut tr) (fullListToQMCut bl) (fullListToQMCut br)

fullListToQMCut :: Eq a => [[a]] -> QMatrix a
fullListToQMCut [[a]] = Leaf a 1
fullListToQMCut list = let sizeOfMatrix = assignLower (length list)
                           sizeOfQuarter = sizeOfMatrix `div` 2
                           new_list = map (\x -> take sizeOfMatrix x) (take sizeOfMatrix list)
                           (t, b) = splitAt sizeOfQuarter new_list
                           (tl, tr) = unzip $ map (\x -> splitAt sizeOfQuarter x) t
                           (bl, br) = unzip $ map (\x -> splitAt sizeOfQuarter x) b
                       in fold_once $ Branch (fullListToQMCut tl) (fullListToQMCut tr) (fullListToQMCut bl) (fullListToQMCut br)

fullListToQM :: (Eq a, Semiring a) => [[a]] -> QMatrix a
fullListToQM [[a]] = Leaf a 1
fullListToQM list = let sizeOfMatrix = assignUpper (length list)
                        sizeOfQuarter = sizeOfMatrix `div` 2
                        new_list = (map (\x -> x ++ replicate (sizeOfMatrix - length list) zero) list) ++ replicate (sizeOfMatrix - length list) (replicate sizeOfMatrix zero)
                        (t, b) = splitAt sizeOfQuarter new_list
                        (tl, tr) = unzip $ map (\x -> splitAt sizeOfQuarter x) t
                        (bl, br) = unzip $ map (\x -> splitAt sizeOfQuarter x) b
                    in fold_once $ Branch (fullListToQMCut tl) (fullListToQMCut tr) (fullListToQMCut bl) (fullListToQMCut br) 
