module Language where

-- Identifier of variables are strings.
type Identifier = String

-- All operators are strings.
type Operator   = String

-- A term is either a variable or an application of an operator on a list of terms.
data Term       = Var Identifier | Fn Operator [Term] deriving Eq

instance Show Term where
  show (Var v) = v
  show (Fn f args) | length args > 0 = f ++ "(" ++ (format (show args)) ++ ")"
                   | otherwise = f
    where format = filter (\c -> c /= '[' && c /= ']' && c /= '\"')

-- list of variables in a term
varsInTerm :: Term -> [Identifier]
varsInTerm (Var i)     = [i]
varsInTerm (Fn f args) = foldr (++) [] (map (varsInTerm) args)
