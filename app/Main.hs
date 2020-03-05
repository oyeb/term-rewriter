module Main where

import Language
import Match
import Unify
import Derive

vw = Var "w"
vx = Var "x"
vy = Var "y"
vz = Var "z"
symbols = [vw, vx, vy, vz]

zero = Fn "0" []
s1 = Fn "f" [vx, vy, vx]
s2 = Fn "f" [Fn "g" [vy, vy],
             Fn "h" [vz],
             Fn "g" [vw, Fn "h" [zero]]]

suck x = Fn "succ" [x]
addFn a b = Fn "add" [a, b]
mulFn a b = Fn "mul" [a, b]
one = suck zero
two = suck one
three = suck two
four = suck three

rules = [ Rule "add_zero" (addFn vx zero) (vx)
        , Rule "add"      (addFn vy (suck vx)) (addFn (suck vy) vx)
        , Rule "mul_zero" (mulFn vx zero) (zero)
        , Rule "mul"      (mulFn vy (suck vx)) (addFn (mulFn vy vx) vy)]

t = addFn (mulFn two one) (addFn one one)

-- changes x -> fog(y)
dummySubst :: Substitution
dummySubst "x" = Fn "fog" [Var "x"]
dummySubst x = identity x

main :: IO ()
main = do
  putStrLn $ show s1
  putStrLn $ show s2
  putStrLn $ show $ applySubst dummySubst s1
  putStrLn $ show $ applySubst (head $ unify s1 s2) s2
  putStrLn $ show $ showSubst (head $ unify s1 s2) symbols
  putStrLn "\nRules:"
  showRules rules
  putStrLn $ "\nTerm: " ++ show t ++ "\n"
  showDerivation $ head $ derive rules t
  
