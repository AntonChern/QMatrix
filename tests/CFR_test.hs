module CFR_test where

import CFR
import Data.Map
import QuadtreePar

graph = fromList [('a', (Branch (Branch (Leaf False 1) (Leaf True 1) (Leaf False 1) (Leaf False 1)) (Leaf False 2) (Leaf False 2) (Branch (Leaf False 1) (Leaf True 1) (Leaf False 1) (Leaf False 1)))), ('b', (Branch (Leaf False 2) (Branch (Leaf False 1) (Leaf False 1) (Leaf True 1) (Leaf False 1)) (Branch (Leaf False 1) (Leaf False 1) (Leaf True 1) (Leaf False 1)) (Leaf False 2)))]

grammar = Grammar {alphabet = ['S', 'A', 'B', 'a', 'b'], nonterminals = ['S', 'A', 'B'], rules = [(Complex ('S', ('A', 'B'))), (Simple ('A', 'a')), (Simple ('B', 'b'))], start = 'S'}

answer = Branch (Leaf False 2) (Branch (Leaf True 1) (Leaf False 1) (Leaf False 1) (Leaf False 1)) (Branch (Leaf True 1) (Leaf False 1) (Leaf False 1) (Leaf False 1)) (Leaf False 2)

test :: Bool
test = answer == solve graph grammar
