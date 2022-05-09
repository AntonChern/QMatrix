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
        result = solving ! l + input_graph ! r
    handleEpsSim graph (rule:rules) input_graph = handleEpsSim graph rules input_graph
    handleCom :: Graph Bool -> [Rule] -> Graph Bool
    handleCom graph rules = if changed then (handleCom new_graph rules) else new_graph where
        (new_graph, changed) = handleCom' graph rules False where
        handleCom' :: Graph Bool -> [Rule] -> Bool -> (Graph Bool, Bool)
        handleCom' graph [] changed = (graph, changed)
        handleCom' graph ((Complex (l, (r1, r2))):rules) changed = (insert l result solving, new_changed || (solving ! l /= result)) where
            (solving, new_changed) = handleCom' graph rules changed
            result = solving ! l + solving ! r1 * solving ! r2
        handleCom' graph (rule:rules) changed = handleCom' graph rules changed 
