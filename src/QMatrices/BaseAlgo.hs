module BaseAlgo where

import Grammar
import QMatrix
import Data.Map
import Data.Semiring

type Graph a = Map Char (QMatrix a)

sizeOfGraph :: Graph a -> Int
sizeOfGraph g
    | Data.Map.null g = 0
    | otherwise = sizeOfQM $ snd $ elemAt 0 g 

instance Semigroup Bool where

instance Monoid Bool where
    mempty = False
    mappend = (||)

instance Semiring Bool where
    one = True
    (<.>) = (&&)

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
    
fillDiag :: QMatrix Bool -> QMatrix Bool
fillDiag m = fillDiag' m (sizeOfQM m) where
    fillDiag' :: QMatrix Bool -> Int -> QMatrix Bool
    fillDiag' m 0 = m
    fillDiag' m i = insertToQM i i True (fillDiag' m (i - 1))

solve :: Graph Bool -> Grammar -> QMatrix Bool
solve graph grammar = (handleCom (handleEpsSim result (rules grammar) graph) (rules grammar)) ! (start grammar) where
    result = fromList (Prelude.map (\x -> (x, Leaf False (sizeOfGraph graph))) (nonterminals grammar))
    handleEpsSim :: Graph Bool -> [Rule] -> Graph Bool -> Graph Bool
    handleEpsSim graph [] _ = graph
    handleEpsSim graph ((Epsilon l):rules) input_graph = insert l result solving where
        solving = handleEpsSim graph rules input_graph
        result = fillDiag $ solving ! l
    handleEpsSim graph ((Simple (l, r)):rules) input_graph = insert l result solving where
        solving = handleEpsSim graph rules input_graph
        result = plusMatrix (solving ! l) (input_graph ! r) (||)
    handleEpsSim graph (rule:rules) input_graph = handleEpsSim graph rules input_graph
    handleCom :: Graph Bool -> [Rule] -> Graph Bool
    handleCom graph rules = if changed then (handleCom new_graph rules) else new_graph where
        (new_graph, changed) = handleCom' graph rules False where
        handleCom' :: Graph Bool -> [Rule] -> Bool -> (Graph Bool, Bool)
        handleCom' graph [] changed = (graph, changed)
        handleCom' graph ((Complex (l, (r1, r2))):rules) changed = (insert l result solving, new_changed || (solving ! l /= result)) where
            (solving, new_changed) = handleCom' graph rules changed
            result = plusMatrix (solving ! l) (multMatrix (solving ! r1) (solving ! r2) (||) (&&)) (||)
        handleCom' graph (rule:rules) changed = handleCom' graph rules changed 
    
-- solve :: Graph Bool -> Grammar -> QMatrix Bool
-- solve graph grammar = (handleCom (handleEpsSim result (rules grammar) graph) (rules grammar)) ! (start grammar) where
--     result = fromList (Prelude.map (\x -> (x, Leaf False (sizeOfGraph graph))) (nonterminals grammar))
--     handleEpsSim :: Graph Bool -> [Rule] -> Graph Bool -> Graph Bool
--     handleEpsSim graph [] _ = graph
--     handleEpsSim graph ((Epsilon l):rules) input_graph = insert l result solving where
--         solving = handleEpsSim graph rules input_graph
--         result = fillDiag $ solving ! l
--     handleEpsSim graph ((Simple (l, r)):rules) input_graph = insert l result solving where
--         solving = handleEpsSim graph rules input_graph
--         result = solving ! l + input_graph ! r
--     handleEpsSim graph (rule:rules) input_graph = handleEpsSim graph rules input_graph
--     handleCom :: Graph Bool -> [Rule] -> Graph Bool
--     handleCom graph rules = if changed then (handleCom new_graph rules) else new_graph where
--         (new_graph, changed) = handleCom' graph rules False where
--         handleCom' :: Graph Bool -> [Rule] -> Bool -> (Graph Bool, Bool)
--         handleCom' graph [] changed = (graph, changed)
--         handleCom' graph ((Complex (l, (r1, r2))):rules) changed = (insert l result solving, new_changed || (solving ! l /= result)) where
--             (solving, new_changed) = handleCom' graph rules changed
--             result = solving ! l + solving ! r1 * solving ! r2
--         handleCom' graph (rule:rules) changed = handleCom' graph rules changed 
