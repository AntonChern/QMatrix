module CFR where

import Data.Map
import Data.Semiring
import QuadtreePar

type Nonterminal = Char
type Terminal = Char

data Rule = Epsilon Nonterminal | Simple (Nonterminal, Terminal) | Complex (Nonterminal, (Nonterminal, Nonterminal))

data Grammar = Grammar {alphabet :: [Char], nonterminals :: [Char], rules :: [Rule], start :: Char}

type Graph a = Map Char (QMatrix a)

msize :: Graph a -> Int
msize g
    | Data.Map.null g = 0
    | otherwise = QuadtreePar.size $ snd $ elemAt 0 g 

instance Semigroup Bool where

instance Monoid Bool where
    mempty = False
    mappend = (||)

instance Semiring Bool where
    one = True
    (<.>) = (&&)

fillDiag :: QMatrix Bool -> QMatrix Bool
fillDiag m = fillDiag' m (QuadtreePar.size m) where
    fillDiag' :: QMatrix Bool -> Int -> QMatrix Bool
    fillDiag' m 0 = m
    fillDiag' m i = qinsert i i True (fillDiag' m (i - 1))

solve :: Graph Bool -> Grammar -> QMatrix Bool
solve graph grammar = (handleCom (handleEpsSim result (rules grammar) graph) (rules grammar)) ! (start grammar) where
    result = fromList (Prelude.map (\x -> (x, Leaf False (msize graph))) (nonterminals grammar))
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
    handleCom graph rules = if changed then handleCom graph rules else new_graph where
        (new_graph, changed) = handleCom' graph rules False where
            handleCom' :: Graph Bool -> [Rule] -> Bool -> (Graph Bool, Bool)
            handleCom' graph [] changed = (graph, changed)
            handleCom' graph ((Complex (l, (r1, r2))):rules) changed = (insert l result solving, new_changed || (solving ! l == result)) where
                (solving, new_changed) = handleCom' graph rules changed
                result = solving ! l + solving ! r1 * solving ! r2
            handleCom' graph (rule:rules) changed = handleCom' graph rules changed
