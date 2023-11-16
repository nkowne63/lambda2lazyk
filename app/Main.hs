module Main (main) where

import Lib (LambdaTerm (..), fullEval)

main :: IO ()
main =
  print
    ( fullEval
        -- 不動点コンビネータ
        ( Abs
            "f"
            ( App
                (Abs "x" (App (Var "f") (App (Var "x") (Var "x"))))
                (Abs "y" (App (Var "f") (App (Var "y") (Var "y"))))
            )
        )
        -- 出力が合ってるかは分からない...（多分こんなに長くないと思う）
        -- S(S(S(K(S))(S(K(K))(I)))(K(S(I)(I))))(S(S(K(S))(S(K(K))(I)))(K(S(I)(I))))
    )
