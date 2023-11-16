module Lib
  ( LambdaTerm (..),
    fullEval,
  )
where

data LambdaTerm a
  = Var a
  | App (LambdaTerm a) (LambdaTerm a)
  | Abs a (LambdaTerm a)
  deriving (Show, Eq)

isFreeAppearInLambdaTerm :: (Eq a) => LambdaTerm a -> a -> Bool
isFreeAppearInLambdaTerm (Var x) y = x == y
isFreeAppearInLambdaTerm (App e1 e2) x = isFreeAppearInLambdaTerm e1 x || isFreeAppearInLambdaTerm e2 x
isFreeAppearInLambdaTerm (Abs x e) y = x /= y && isFreeAppearInLambdaTerm e y

-- Evaluation用の中間状態
data LazyTermEv a
  = EvC (LazyTermEv a) (LazyTermEv a)
  | EvS
  | EvK
  | EvI
  | EvAbs a (LazyTermEv a)
  | EvVar a
  deriving (Show, Eq)

intoLazy :: (Eq a) => LambdaTerm a -> LazyTermEv a
intoLazy (Var x) = EvVar x
intoLazy (App e1 e2) = EvC (intoLazy e1) (intoLazy e2)
intoLazy (Abs x e)
  | not (isFreeAppearInLambdaTerm e x) = EvC EvK (intoLazy e)
  | e == Var x = EvI
  | otherwise =
      case e of
        App e1 e2 ->
          EvC
            (EvC EvS (intoLazy (Abs x e1)))
            (intoLazy (Abs x e2))
        Abs y e1 -> EvAbs x (intoLazy (Abs y e1))
        _ -> error "impossible"

isFreeAppearInLazyTermEv :: (Eq a) => LazyTermEv a -> a -> Bool
isFreeAppearInLazyTermEv EvS _ = False
isFreeAppearInLazyTermEv EvK _ = False
isFreeAppearInLazyTermEv EvI _ = False
isFreeAppearInLazyTermEv (EvVar x) y = x == y
isFreeAppearInLazyTermEv (EvC e1 e2) x = isFreeAppearInLazyTermEv e1 x || isFreeAppearInLazyTermEv e2 x
isFreeAppearInLazyTermEv (EvAbs x e) y = x /= y && isFreeAppearInLazyTermEv e y

data LazyTerm a
  = C (LazyTerm a) (LazyTerm a)
  | K
  | S
  | I
  | LVar a
  deriving (Eq)

instance (Show a) => Show (LazyTerm a) where
  show (C e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
  show K = "K"
  show S = "S"
  show I = "I"
  show (LVar x) = show x

evalLazyTermEv :: (Eq a) => LazyTermEv a -> LazyTerm a
evalLazyTermEv EvS = S
evalLazyTermEv EvK = K
evalLazyTermEv EvI = I
evalLazyTermEv (EvVar x) = LVar x
evalLazyTermEv (EvC e1 e2) = C (evalLazyTermEv e1) (evalLazyTermEv e2)
evalLazyTermEv (EvAbs x e)
  | not (isFreeAppearInLazyTermEv e x) = C K (evalLazyTermEv e)
  | e == EvVar x = I
  | otherwise =
      case e of
        EvC e1 e2 ->
          C
            (C S (evalLazyTermEv (EvAbs x e1)))
            (evalLazyTermEv (EvAbs x e2))
        EvAbs y e1 -> evalLazyTermEv (EvAbs x (EvAbs y e1))
        _ -> error "impossible"

fullEval :: (Eq a) => LambdaTerm a -> LazyTerm a
fullEval = evalLazyTermEv . intoLazy