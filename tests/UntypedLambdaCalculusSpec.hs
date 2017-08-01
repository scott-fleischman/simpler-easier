module UntypedLambdaCalculusSpec where

import UntypedLambdaCalculus
import Test.Hspec

[z,s,m,n] = map (Var . (:[])) "zsmn"
app2 f x y = App (App f x) y
zero  = Lam "s" $ Lam "z" z
one   = Lam "s" $ Lam "z" $ App s z
two   = Lam "s" $ Lam "z" $ App s $ App s z
three = Lam "s" $ Lam "z" $ App s $ App s $ App s z
plus  = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ app2 m s (app2 n s z)

spec :: Spec
spec =
  describe "Untyped lambda calculus" $
    it "betaEq" $
      betaEq (app2 plus one two) three `shouldBe` True
