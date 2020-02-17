import Test.Hspec

import Language
import Match
import Unify

main :: IO ()
main = hspec $ do
  describe "language" $ do
    it "can show vars" $ do
      show (Var "x") `shouldBe` "x"
      
    it "can show funcs" $ do
      show (Fn "0" []) `shouldBe` "0"

    it "can show nested funcs" $ do
      show (Fn "f" [Var "x", Fn "neg" [Var "ab"]]) `shouldBe` "f(x,neg(ab))"

  describe "match/subst" $ do
    let sigma = makeSubst "sum_function" (Fn "sum" [Var "xs"])
    describe "makeSubst" $ do
      it "works" $ do
        sigma "sum_function" `shouldBe` (Fn "sum" [Var "xs"])

    it "substitutions can handle stuff not in domain" $ do
      sigma "g" `shouldBe` Var "g"

  describe "unify" $ do
    let
      vw = Var "w"
      vx = Var "x"
      vy = Var "y"
      vz = Var "z"

      zero = Fn "0" []
      s1 = Fn "f" [vx, vy, vx]
      s2 = Fn "f" [Fn "g" [vy, vy],
                   Fn "h" [vz],
                   Fn "g" [vw, Fn "h" [zero]]]
    it "unifier of s1 and s2 exists" $ do
      length (unify s1 s2) `shouldBe` 1
    it "unifies s1 and s2 correctly" $ do
      applySubst (head $ unify s1 s2) s1 `shouldBe` Fn "f" [Fn "g" [Fn "h" [zero],
                                                                    Fn "h" [zero]],
                                                            Fn "h" [zero],
                                                            Fn "g" [Fn "h" [zero],
                                                                    Fn "h" [zero]]]

