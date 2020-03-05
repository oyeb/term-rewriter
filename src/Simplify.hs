module Simplify (simplify) where

import Data.List -- might need the `nub` function.
import Language
import Match
import Unify

-- Takes a Rule set and a ground term and computes the normalized form of the
-- term.
-- Applies rules (left to right) till no more rules can be applied.
-- Returns a list of normal forms.
simplify :: [Rule] -> Term -> [Term]
simplify rules term = simplify' rules [term]

-- This is a hint, a function that simplifies a list of terms as "deeply" as possible recursively till no rule applies.
simplify' :: [Rule] -> [Term] -> [Term]
simplify' rules ts = let ts' = nub . concat $ map (deepApply rules) ts
                     in if length ts' == 0 then ts else simplify' rules ts'

-- This is a hint, a function that applies rules to a term, not it's sub-terms.
applyOnce :: [Rule] -> Term -> [Term]
applyOnce rules term = [ applySubst sigma (rhs r) |
                         r <- rules,
                         sigma <- unify (lhs r) term ]

-- This is a hint, a function that can recusively apply rules on a term AND it's
-- sub-terms, and return their normal forms.
deepApply :: [Rule] -> Term -> [Term]
deepApply rules term@(Fn f args) = applyOnce rules term ++ [
  Fn f (left ++ (t':tail)) |
  (left, (t:tail)) <- chunks args,
  t' <- deepApply rules t ]

-- A function that the TA found to be useful
chunks :: [a] -> [([a], [a])]
chunks [] = [] -- not [([], [])]
chunks t@(x:xs) = ([],t):(map (\(h,t) -> (x:h,t)) $ chunks xs)
