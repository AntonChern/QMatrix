{-# LANGUAGE FlexibleInstances #-}
module SemiringAlgo where

import Grammar
import QMatrix
import Data.Semiring
import Data.Map
import Data.List

type Graph = QMatrix [NonTerm]
type NewGraph = QMatrix Set

data Set = Set ([NonTerm], [Rule]) deriving Eq

instance Semigroup Set where

instance Monoid Set where
    mempty = Set ([], [])
    mappend (Set (a, rules1)) (Set (b, rules2)) = Set (sort $ Data.List.union a b, Data.List.union rules1 rules2)
    
instance Semiring Set where
    (Set (a, rules1)) <.> (Set (b, rules2)) = Set (operation_grammar a b rules1, rules1) where
        operation_grammar :: [NonTerm] -> [NonTerm] -> [Rule] -> [NonTerm]
        operation_grammar non1 non2 rules = [l | (Complex (l, (r1, r2))) <- rules, n1 <- non1, n2 <- non2, r1 == n1 && r2 == n2]

solve :: Graph -> Grammar -> QMatrix Bool
solve graph grammar = transform (solve_e' (convert graph (rules grammar))) (start grammar)-- where
solve_e' :: NewGraph -> NewGraph
solve_e' graph = if graph == res then graph else solve_e' res where
    res = graph * graph + graph
transform :: NewGraph -> NonTerm -> QMatrix Bool
transform (Leaf (Set (n, _)) k) start = Leaf (elem start n) k
transform (Branch tl tr bl br) start = Branch (transform tl start) (transform tr start) (transform bl start) (transform br start)

convert :: Graph -> [Rule] -> NewGraph
convert (Leaf l k) rules = Leaf (Set (l, rules)) k
convert (Branch tl tr bl br) rules = Branch (convert tl rules) (convert tr rules) (convert bl rules) (convert br rules)
    
reconvert :: NewGraph -> Graph
reconvert (Leaf (Set (l,_)) k) = Leaf l k
reconvert (Branch tl tr bl br) = Branch (reconvert tl) (reconvert tr) (reconvert bl) (reconvert br)

one_step :: Graph -> Grammar -> Graph
one_step (Leaf n k) grammar = Leaf (nub $ [l | (Simple (l, x)) <- (rules grammar), y <- n, x == y] ++ [l | (Epsilon l) <- (rules grammar)]) k
one_step (Branch tl tr bl br) grammar = fold_once $ Branch (one_step tl grammar) (one_step tr grammar) (one_step bl grammar) (one_step br grammar)
 
