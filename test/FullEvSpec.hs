module FullEvSpec (spec) where

import Lib (LambdaTerm (..), fullEval)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "fullEval: test" $ do
    it "sample" $ do
      show (fullEval (Abs "x" (Abs "y" (App (Var "y") (Var "x"))))) `shouldBe` "S(K(S(I)))(S(K(K))(I))"
