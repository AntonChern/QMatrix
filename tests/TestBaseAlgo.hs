module Main where

import QMatrix
import Grammar
import BaseAlgo
import Criterion.Main
import Data.Map
import Data.Char
import Data.List
import Data.Time
import Control.Monad

graph_bintree = fromList [('a', Branch (Branch (Leaf False 2) (Branch (Leaf True 1) (Leaf False 1) (Leaf True 1) (Leaf False 1)) (Leaf False 2) (Leaf False 2)) (Branch (Leaf False 2) (Leaf False 2) (Branch (Leaf False 1) (Leaf False 1) (Leaf False 1) (Leaf True 1)) (Branch (Leaf True 1) (Leaf False 1) (Leaf False 1) (Leaf False 1))) (Leaf False 4) (Branch (Branch (Leaf False 1) (Leaf True 1) (Leaf False 1) (Leaf False 1)) (Branch (Leaf False 1) (Leaf False 1) (Leaf True 1) (Leaf False 1)) (Leaf False 2) (Leaf False 2))), ('b', (Branch (Branch (Leaf False 2) (Leaf False 2) (Branch (Leaf True 1) (Leaf True 1) (Leaf False 1) (Leaf False 1)) (Leaf False 2)) (Leaf False 4) (Branch (Leaf False 2) (Branch (Leaf False 1) (Leaf False 1) (Leaf False 1) (Leaf True 1)) (Leaf False 2) (Branch (Leaf True 1) (Leaf False 1) (Leaf False 1) (Leaf False 1))) (Branch (Branch (Leaf False 1) (Leaf False 1) (Leaf True 1) (Leaf False 1)) (Leaf False 2) (Branch (Leaf False 1) (Leaf True 1) (Leaf False 1) (Leaf False 1)) (Leaf False 2))))]

grammar_bintree = grammar_2

answer_bintree = fullListToQM [[True, True, False,True, True ,False,False,False],
                               [True, True, False,True, True ,False,False,False],
                               [False,False,True, False,False,True, False,False],
                               [True, True, False,True, True ,False,False,False],
                               [True, True, False,True, True ,False,False,False],
                               [False,False,True, False,False,True, False,False],
                               [False,False,False,False,False,False,False,False],
                               [False,False,False,False,False,False,False,False]]

graph_cycle = fromList [('a',fullListToQMWith [[False,True,False],[False,False,True],[True,False,False]] False)]

grammar_cycle = grammar_1

answer_cycle = fullListToQMWith [[True,True,True],
                                 [True,True,True],
                                 [True,True,True]] False

graph_line = fromList [('a',fullListToQMWith [[False,True,False,False,False],
                                              [False,False,True,False,False],
                                              [False,False,False,False,False],
                                              [False,False,False,False,False],
                                              [False,False,False,False,False]] False),
                       ('b',fullListToQMWith [[False,False,False,False,False],
                                              [False,False,False,False,False],
                                              [False,False,False,True,False],
                                              [False,False,False,False,True],
                                              [False,False,False,False,False]] False)]
                         
grammar_line = grammar_2

answer_line = fullListToQMWith [[False,False,False,False,True ],
                                [False,False,False,True, False],
                                [False,False,False,False,False],
                                [False,False,False,False,False],
                                [False,False,False,False,False]] False                


graph_loop = fromList [('a',fullListToQMWith [[True,False,False],[False,False,False],[False,False,False]] False)]

grammar_loop = grammar_1

answer_loop = fullListToQMWith [[True,False,False],[False,False,False],[False,False,False]] False


graph_two_cycles = fromList [('a',fullListToQM [[False,True,False,False],[False,False,True,False],[True,False,False,False],[False,False,False,False]]),('b',fullListToQM [[False,False,False,False],[False,False,False,False],[False,False,False,True],[False,False,True,False]])]

grammar_two_cycles = grammar_2

answer_two_cycles = fullListToQM [[False,False,True,True],[False,False,True,True],[False,False,True,True],[False,False,False,False]]


grammar_1 = Grammar {terminals = ['a'], nonterminals = ['S', 'A'], rules = [(Complex ('S', ('A', 'S'))), (Simple ('A', 'a')), (Simple ('S', 'a'))], start = 'S'}

grammar_2 = Grammar {terminals = ['a', 'b'], nonterminals = ['S', 'T', 'A', 'B'], rules = [(Complex ('S', ('A', 'T'))), (Complex ('S', ('A', 'B'))), (Complex ('T', ('S', 'B'))), (Simple ('A', 'a')), (Simple ('B', 'b'))], start = 'S'}

prepareGraph :: String -> Grammar -> Graph Bool
prepareGraph string grammar = let list = Prelude.map words (lines string)
                              in fromList $ Prelude.map (\t -> (t, listToQM $ Prelude.map (\(i:j:[a]) -> ((read i :: Int, read j :: Int), True)) (Prelude.filter (\(i:j:[a]) -> (Prelude.map (toLower) a) == [t]) list))) (terminals grammar)

-- split_lists :: (a -> Bool) -> [[a]] -> ([((Int, Int), Bool)], [((Int, Int), Bool)])
-- split_lists _ [] = ([], [])
-- split_lists p ((i:j:[x]):xs) = if p x then (((read i :: Int, read j :: Int), True):a, d) else (a, ((read i :: Int, read j :: Int), True):d) where
--     (a, d) = split_lists p xs
--                               
-- prepareGraph :: String -> Grammar -> Graph Bool
-- prepareGraph string grammar = let list = Prelude.map words (lines string)
--                                   (a_list, d_list) = split_lists (== "a") list
--                               in fromList $ [('a', listToQM $ a_list), (listToQM $ d_list)]

main :: IO ()
main = do
--     string <- readFile "gzip.csv"
--     let graph_big = prepareGraph string grammar_big
    defaultMain [
        bgroup "base" [bench "bin_tree" $ whnf (\(x, y) -> answer_bintree == solve x y) (graph_bintree, grammar_bintree)
                     , bench "cycle"  $ whnf (\(x, y) -> answer_cycle == solve x y) (graph_cycle, grammar_cycle)
                     , bench "line" $ whnf (\(x, y) -> answer_line == solve x y) (graph_line, grammar_line)
                     , bench "loop" $ whnf (\(x, y) -> answer_loop == solve x y) (graph_loop, grammar_loop)
                     , bench "two_cycles" $ whnf (\(x, y) -> answer_two_cycles == solve x y) (graph_two_cycles, grammar_two_cycles)
                ]--,
--         bgroup "big_data" [bench "example" $ whnf (\(x, y) -> solve x y) (graph_big, grammar_big)
--                 ]
        ]

getTimes :: [Double] -> Int -> IO ([Double])
getTimes x _ = do
    string <- readFile "ls.csv"
    let graph_big = prepareGraph string grammar_big
    start <- getCurrentTime
    writeFile "writer.txt" (show $ solve graph_big grammar_big)
    stop <- getCurrentTime
    return ((readDouble (show $ diffUTCTime stop start)) : x)

stddevs :: [Double] -> Double -> Double
stddevs [] _ = 0.0
stddevs (x:xs) y = (x - y) ** 2 + stddevs xs y
    
readDouble :: String -> Double
readDouble x = read (reverse (tail (reverse x))) :: Double
    
-- main :: IO ()
-- main = do
--     times <- foldM getTimes [] [1..2]
--     iterations <- return (read (show $ length times) :: Double)
--     mean <- return $ (sum times) / iterations
--     print (times)
--     putStrLn $ show (mean)
--     print (sqrt $ (stddevs times mean) / iterations)

grammar_big = Grammar {terminals = ['a', 'd'], nonterminals = ['S', 'A', 'R', 'V', 'D', 'B', 'T', 'C', 'E', 'H', 'P'], rules = [(Simple ('A', 'a')),(Simple ('E', 'a')),(Simple ('T', 'a')),(Simple ('V', 'a')),(Simple ('P', 'a')),(Simple ('D', 'd')),(Simple ('R', 'd')),(Simple ('H', 'd')),(Simple ('B', 'd')),(Complex ('S', ('A', 'R'))),(Complex ('R', ('V', 'D'))),(Complex ('V', ('B', 'T'))),(Complex ('T', ('C', 'E'))),(Complex ('B', ('C', 'H'))),(Complex ('H', ('D', 'B'))),(Complex ('E', ('A', 'P'))),(Complex ('P', ('C', 'E'))),(Complex ('V', ('C', 'E'))),(Complex ('V', ('A', 'P'))),(Complex ('V', ('A', 'R'))),(Complex ('T', ('A', 'P'))),(Complex ('T', ('A', 'R'))),(Complex ('B', ('D', 'B'))),(Complex ('C', ('A', 'R'))),(Complex ('P', ('A', 'P'))),(Complex ('P', ('A', 'R')))], start = 'S'}
