module QMatrix where

import Data.Semiring
import GHC.Float

data QMatrix a = Leaf a Int | Branch (QMatrix a) (QMatrix a) (QMatrix a) (QMatrix a)
    deriving (Eq, Show)

-- instance Show a => Show (QMatrix a) where
--     show m = "\n" ++ show' res (maxLen res) where
--         res = toList m
-- show' :: [[String]] -> Int -> String
-- show' [x] k = showCool x k ++ "\n"
-- show' (x:xs) k = showCool x k ++ "\n" ++ (show' xs k)
-- toList :: Show a => QMatrix a -> [[String]]
-- toList (Leaf a k) = replicate k (replicate k (show a))
-- toList (Branch tl tr bl br) = (zipWith (++) (toList tl) (toList tr)) ++ (zipWith (++) (toList bl) (toList br))
-- showCool :: [String] -> Int -> String
-- showCool [x] _ = x
-- showCool (x:xs) k = x ++ (replicate (k - length x) ' ') ++ "|" ++ (showCool xs k)
-- maxLen :: [[String]] -> Int
-- maxLen list = maximum $ map maxLen' list where
--     maxLen' :: [String] -> Int
--     maxLen' list = maximum $ map length list

sizeOfQM :: QMatrix a -> Int
sizeOfQM (Leaf _ k) = k
sizeOfQM (Branch tl tr bl br) = 2 * sizeOfQM tl

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

fold :: Eq a => QMatrix a -> QMatrix a
fold (Leaf a k) = Leaf a k
fold (Branch tl tr bl br) = fold_once $ Branch (fold tl) (fold tr) (fold bl) (fold br)

mismatchError = error "Mismatch of sizes"

instance (Eq a, Semiring a) => Num (QMatrix a) where
    (Leaf a k1) + (Leaf b k2)
        | k1 == k2 = Leaf (a <+> b) k1
        | otherwise = mismatchError
    (Branch tl tr bl br) + (Leaf a k)
        | k >= 2 = Branch (tl + Leaf a (k `div` 2)) (tr + Leaf a (k `div` 2)) (bl + Leaf a (k `div` 2)) (br + Leaf a (k `div` 2))
        | otherwise = mismatchError
    (Leaf a k) + (Branch tl tr bl br)
        | k >= 2 = Branch (Leaf a (k `div` 2) + tl) (Leaf a (k `div` 2) + tr) (Leaf a (k `div` 2) + bl) (Leaf a (k `div` 2) + br)
        | otherwise = mismatchError
    (Branch tl1 tr1 bl1 br1) + (Branch tl2 tr2 bl2 br2) = fold_once $ Branch (tl1 + tl2) (tr1 + tr2) (bl1 + bl2) (br1 + br2)
    (Leaf a k1) * (Leaf b k2)
        | k1 == k2 = if k1 == 1 then Leaf (a <.> b) 1 else let res = Leaf a (k1 `div` 2) * Leaf b (k2 `div` 2) in let e = res + res in fold_once $ Branch e e e e
        | otherwise = mismatchError
    (Branch tl tr bl br) * (Leaf a k)
        | k >= 2 = fold_once $ Branch (tl * Leaf a (k `div` 2) + tr * Leaf a (k `div` 2)) (tl * Leaf a (k `div` 2) + tr * Leaf a (k `div` 2)) (bl * Leaf a (k `div` 2) + br * Leaf a (k `div` 2)) (bl * Leaf a (k `div` 2) + br * Leaf a (k `div` 2))
        | otherwise = mismatchError
    (Leaf a k) * (Branch tl tr bl br)
        | k >= 2 = fold_once $ Branch (Leaf a (k `div` 2) * tl + Leaf a (k `div` 2) * bl) (Leaf a (k `div` 2) * tr + Leaf a (k `div` 2) * br) (Leaf a (k `div` 2) * tl + Leaf a (k `div` 2) * bl) (Leaf a (k `div` 2) * tr + Leaf a (k `div` 2) * br)
        | otherwise = mismatchError
    (Branch tl1 tr1 bl1 br1) * (Branch tl2 tr2 bl2 br2) = fold_once $ Branch (tl1 * tl2 + tr1 * bl2) (tl1 * tr2 + tr1 * br2) (bl1 * tl2 + br1 * bl2) (bl1 * tr2 + br1 * br2)
    
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
