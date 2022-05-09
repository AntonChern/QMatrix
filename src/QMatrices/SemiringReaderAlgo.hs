module SemiringReaderAlgo where

import Grammar
import QMatrix
import Data.Semiring
import Control.Monad.Reader
import Data.List (nub)
import GHC.Float


type AdvNon = (Reader [Rule] [NonTerm])

instance Eq AdvNon where
    x == y = False

instance Semigroup AdvNon where
  x <> y = do
    x <- x
    y <- y
    return (nub $ x ++ y)

instance Monoid AdvNon where
  mempty = return []

instance Semiring AdvNon where
  one = undefined
  x <.> y = do
    x <- x
    y <- y
    rules <- ask
    return [ lhs | (Complex (lhs, (rhs1, rhs2))) <- rules,
                   n1 <- x,
                   n2 <- y,
                   rhs1 == n1 && rhs2 == n2
           ]
           
type Graph = QMatrix [NonTerm]
type AdvGraph = QMatrix AdvNon

convert :: Graph -> AdvGraph
convert (Leaf a k) = Leaf (return a) k
convert (Branch tl tr bl br) = Branch (convert tl) (convert tr) (convert bl) (convert br)
    
reconvert :: AdvGraph -> [Rule] -> Graph
reconvert (Leaf a k) rules = Leaf (runReader a rules) k
reconvert (Branch tl tr bl br) rules = Branch (reconvert tl rules) (reconvert tr rules) (reconvert bl rules) (reconvert br rules)

transitiveClosure :: Graph -> [Rule] -> Graph
transitiveClosure x rules = if x == y then y else transitiveClosure y rules where
    y = reconvert ((convert x) * (convert x) + (convert x)) rules

{-Long-}

-- log' :: Float -> Int
-- log' 1 = 1
-- log' x = truncate $ logBase 2 (2 * (x - 1))
--
-- transitiveClosure :: AdvGraph -> AdvGraph
-- transitiveClosure graph = transitiveClosure' graph (log' $ int2Float (size graph)) where
--     transitiveClosure' :: AdvGraph -> Int -> AdvGraph
--     transitiveClosure' graph 0 = graph
--     transitiveClosure' graph k = transitiveClosure' (graph * graph + graph) (k - 1)
--
-- solve :: Graph -> Grammar -> QMatrix Bool
-- solve graph grammar = transform (reconvert (transitiveClosure (convert graph)) (rules grammar)) (start grammar)
    
solve :: Graph -> Grammar -> QMatrix Bool
solve graph grammar = transform (transitiveClosure graph (rules grammar)) (start grammar)

transform :: Graph -> NonTerm -> QMatrix Bool
transform (Leaf a k) start = Leaf (elem start a) k
transform (Branch tl tr bl br) start = Branch (transform tl start) (transform tr start) (transform bl start) (transform br start)

one_step :: Graph -> Grammar -> Graph
one_step (Leaf n k) grammar = Leaf (nub $ [l | (Simple (l, x)) <- (rules grammar), y <- n, x == y] ++ [l | (Epsilon l) <- (rules grammar)]) k
one_step (Branch tl tr bl br) grammar = fold_once $ Branch (one_step tl grammar) (one_step tr grammar) (one_step bl grammar) (one_step br grammar)
