module Main (main) where

import Lib (LambdaTerm (..), fullEval)

main :: IO ()
main = print (fullEval (Abs "x" (Abs "y" (App (Var "x") (Var "y")))))
