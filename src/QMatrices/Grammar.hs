module Grammar where

type NonTerm = Char
type Term = Char

data Rule = Epsilon NonTerm | Simple (NonTerm, Term) | Complex (NonTerm, (NonTerm, NonTerm)) deriving Eq

data Grammar = Grammar {terminals :: [Term], nonterminals :: [NonTerm], rules :: [Rule], start :: NonTerm}
