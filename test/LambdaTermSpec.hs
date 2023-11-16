module LambdaTermSpec (spec) where

import Lib (LambdaTerm (..), isFreeAppearInLambda)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "isFreeAppearInLambda: true case" $ do
    it "Var" $ do
      isFreeAppearInLambda (Var "a") "a" `shouldBe` True
    it "App" $ do
      isFreeAppearInLambda (App (Var "a") (Var "b")) "a" `shouldBe` True
    it "Abs" $ do
      isFreeAppearInLambda (Abs "a" (Var "b")) "b" `shouldBe` True
