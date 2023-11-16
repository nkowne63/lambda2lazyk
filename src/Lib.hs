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
intoLazy (Abs x (App e1 e2)) = EvC (EvC EvS (intoLazy (Abs x e1))) (intoLazy (Abs x e2))
intoLazy (Abs x (Abs y e)) = EvAbs x (intoLazy (Abs y e))
intoLazy (Abs x e) = if e == Var x then EvI else EvC EvK (intoLazy e)

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
  deriving (Show, Eq)

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