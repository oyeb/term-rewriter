module Derive where

import Data.List -- might need the `nub` function.
import Language
import Match
import Unify

-- Operates like `simplify` but additionally report the steps taken
-- to reach the normal form.
--
-- Each derivation (list of steps) is recorded in reverse, ie. most recent step
-- is at the head of the list.
derive :: [Rule] -> Term -> [Derivation]
derive rules term = derive' rules [[(term, Assume)]]

-- This is a hint. Very similar to Simplify.
derive' :: [Rule] -> [Derivation] -> [Derivation]
derive' rules ds = let ds' = nub . concat $ map (deepDerive rules) ds
                   in if length ds' == 0 then ds else derive' rules ds'

deriveOnce :: [Rule] -> Derivation -> [Derivation]
deriveOnce rules ((term, r):steps) = [ (applySubst sigma (rhs r'), r'):(term, r):steps |
                                       r' <- rules,
                                       sigma <- unify (lhs r') term ]

-- This is a hint, a function that can recusively apply rules on a term AND it's
-- sub-terms, and return their normal forms.
deepDerive :: [Rule] -> Derivation -> [Derivation]
deepDerive rules d@((term@(Fn f args), r):steps) =
  deriveOnce rules d ++
  [ ((Fn f (left ++ (t':tail)), r'):steps') |
    (left, (t:tail)) <- chunks args,
    ((t', r'):steps') <- deepDerive rules ((t, Assume):(term, r):steps) ]

-- A function that the TA found to be useful
chunks :: [a] -> [([a], [a])]
chunks [] = [] -- not [([], [])]
chunks t@(x:xs) = ([],t):(map (\(h,t) -> (x:h,t)) $ chunks xs)
