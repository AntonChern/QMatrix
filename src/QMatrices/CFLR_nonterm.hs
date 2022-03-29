module CFLR_nonterm where

import Data.Map
import Data.List
import Data.Semiring
import QuadtreePar
import Control.DeepSeq
import Control.Monad.Par hiding (NFData)

type Nonterminal = Char
type Terminal = Char

data Rule = Epsilon Nonterminal | Simple (Nonterminal, Terminal) | Complex (Nonterminal, (Nonterminal, Nonterminal)) deriving Eq

instance NFData Rule where
    rnf m = seq m ()

data Grammar = Grammar {alphabet :: [Char], nonterminals :: [Char], rules :: [Rule], start :: Char}

-- type Graph a = Map Char (QMatrix a)

type Graph = QMatrix [Nonterminal]
type NewGraph = QMatrix Set

data Set = Set ([Nonterminal], [Rule]) deriving Eq

instance NFData Set where
    rnf (Set (n, rules)) = rnf n `seq` rnf rules

instance Semigroup Set where

instance Monoid Set where
    mempty = Set ([], [])
    mappend (Set (a, rules1)) (Set (b, rules2)) = Set (sort $ Data.List.union a b, Data.List.union rules1 rules2)
    
instance Semiring Set where
    (Set (a, rules1)) <.> (Set (b, rules2)) = Set (operation_grammar a b rules1, rules1) where
        operation_grammar :: [Nonterminal] -> [Nonterminal] -> [Rule] -> [Nonterminal]
        operation_grammar non1 non2 rules = [l | (Complex (l,(r1,r2))) <- rules, n1 <- non1, n2 <- non2, r1 == n1 && r2 == n2]

solve :: Graph -> Grammar -> QMatrix Bool
solve graph grammar = transform (solve' (convert graph (rules grammar))) (start grammar) where
    solve' :: NewGraph -> NewGraph
    solve' graph = if graph == res then graph else solve' res where
        res = graph * graph + graph
    transform :: NewGraph -> Nonterminal -> QMatrix Bool
    transform (Leaf (Set (n, _)) k) start = Leaf (elem start n) k
    transform (Branch tl tr bl br) start = Branch (transform tl start) (transform tr start) (transform bl start) (transform br start)

convert :: Graph -> [Rule] -> NewGraph
convert (Leaf l k) rules = Leaf (Set (l, rules)) k
convert (Branch tl tr bl br) rules = Branch (convert tl rules) (convert tr rules) (convert bl rules) (convert br rules)
    
reconvert :: NewGraph -> Graph
reconvert (Leaf (Set (l,_)) k) = Leaf l k
reconvert (Branch tl tr bl br) = Branch (reconvert tl) (reconvert tr) (reconvert bl) (reconvert br)
