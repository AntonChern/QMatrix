module Main where

import QMatrix
import Grammar
import GeneralAlgo
import Criterion.Main
import Data.Char
import Data.Time
import Control.Monad

graph_bintree = fullListToQMCut [[[],   [],   ['A'],[],   [],   [],   [],   []],
                                 [[],   [],   ['A'],[],   [],   [],   [],   []],
                                 [['B'],['B'],[],   [],   [],   [],   ['A'],[]],
                                 [[],   [],   [],   [],   [],   ['A'],[],   []],
                                 [[],   [],   [],   [],   [],   ['A'],[],   []],
                                 [[],   [],   [],   ['B'],['B'],[],   ['A'],[]],
                                 [[],   [],   ['B'],[],   [],   ['B'],[],   []],
                                 [[],   [],   [],   [],   [],   [],   [],   []]]

grammar_bintree = grammar_2

answer_bintree = fullListToQMCut [[True, True, False,True, True ,False,False,False],
                                  [True, True, False,True, True ,False,False,False],
                                  [False,False,True, False,False,True, False,False],
                                  [True, True, False,True, True ,False,False,False],
                                  [True, True, False,True, True ,False,False,False],
                                  [False,False,True, False,False,True, False,False],
                                  [False,False,False,False,False,False,False,False],
                                  [False,False,False,False,False,False,False,False]]

graph_cycle = fullListToQMWith [[[],       ['S','A'],[]       ],
                                [[],       [],       ['S','A']],
                                [['S','A'],[],       []      ]] []

grammar_cycle = grammar_1

answer_cycle = fullListToQMWith [[True,True,True],
                                 [True,True,True],
                                 [True,True,True]] False


graph_line = fullListToQMWith [[[],   ['A'],[],   [],   []   ],
                               [[],   [],   ['A'],[],   []   ],
                               [[],   [],   [],   ['B'],[]   ],
                               [[],   [],   [],   [],   ['B']],
                               [[],   [],   [],   [],   []   ]] []
                         
grammar_line = grammar_2

answer_line = fullListToQMWith [[False,False,False,False,True ],
                                [False,False,False,True, False],
                                [False,False,False,False,False],
                                [False,False,False,False,False],
                                [False,False,False,False,False]] False


graph_loop = fullListToQMWith [[['S','A'],[],[]],[[],[],[]],[[],[],[]]] []

grammar_loop = grammar_1

answer_loop = fullListToQMWith [[True,False,False],[False,False,False],[False,False,False]] False


graph_two_cycles = fullListToQMCut [[[],['A'],[],[]],[[],[],['A'],[]],[['A'],[],[],['B']],[[],[],['B'],[]]]

grammar_two_cycles = grammar_2

answer_two_cycles = fullListToQMCut [[False,False,True,True],[False,False,True,True],[False,False,True,True],[False,False,False,False]]


grammar_1 = Grammar {terminals = ['a'], nonterminals = ['S', 'A'], rules = [(Complex ('S', ('A', 'S'))), (Simple ('A', 'a')), (Simple ('S', 'a'))], start = 'S'}

grammar_2 = Grammar {terminals = ['a', 'b'], nonterminals = ['S', 'T', 'A', 'B'], rules = [(Complex ('S', ('A', 'T'))), (Complex ('S', ('A', 'B'))), (Complex ('T', ('S', 'B'))), (Simple ('A', 'a')), (Simple ('B', 'b'))], start = 'S'}

prepareGraph :: String -> Grammar -> Graph
prepareGraph string grammar = let list = map words (lines string)
                                  matrix = listToQMWith (map (\(i:j:[a]) -> ((read i :: Int, read j :: Int), map toLower a)) list) []
                              in one_step matrix grammar

main :: IO ()
main = do
     string <- readFile "bzip.csv"
     let graph_big = prepareGraph string grammar_big
     defaultMain [
        bgroup "base" [bench "bin_tree" $ whnf (\(x, y) -> answer_bintree == solve x y) (graph_bintree, grammar_bintree)
               , bench "cycle"  $ whnf (\(x, y) -> answer_cycle == solve x y) (graph_cycle, grammar_cycle)
               , bench "line" $ whnf (\(x, y) -> answer_line == solve x y) (graph_line, grammar_line)
               , bench "loop" $ whnf (\(x, y) -> answer_loop == solve x y) (graph_loop, grammar_loop)
               , bench "two_cycles" $ whnf (\(x, y) -> answer_two_cycles == solve x y) (graph_two_cycles, grammar_two_cycles)
               ],
        bgroup "big_data" [bench "example" $ whnf (\(x, y) -> solve x y) (graph_big, grammar_big)
                   ]
        ]

grammar_big = Grammar {terminals = ['a', 'd'], nonterminals = ['S', 'A', 'R', 'V', 'D', 'B', 'T', 'C', 'E', 'H', 'P'], rules = [(Simple ('A', 'a')),(Simple ('E', 'a')),(Simple ('T', 'a')),(Simple ('V', 'a')),(Simple ('P', 'a')),(Simple ('D', 'd')),(Simple ('R', 'd')),(Simple ('H', 'd')),(Simple ('B', 'd')),(Complex ('S', ('A', 'R'))),(Complex ('R', ('V', 'D'))),(Complex ('V', ('B', 'T'))),(Complex ('T', ('C', 'E'))),(Complex ('B', ('C', 'H'))),(Complex ('H', ('D', 'B'))),(Complex ('E', ('A', 'P'))),(Complex ('P', ('C', 'E'))),(Complex ('V', ('C', 'E'))),(Complex ('V', ('A', 'P'))),(Complex ('V', ('A', 'R'))),(Complex ('T', ('A', 'P'))),(Complex ('T', ('A', 'R'))),(Complex ('B', ('D', 'B'))),(Complex ('C', ('A', 'R'))),(Complex ('P', ('A', 'P'))),(Complex ('P', ('A', 'R')))], start = 'S'}
