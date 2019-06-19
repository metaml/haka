{-# language FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeOperators #-}
module Free where

import Control.Monad (forever)

-- intuitive but rigid that directly leads to the "expression problem"
data Expr' = Val' Int | Add' Expr' Expr'

eval' :: Expr' -> Int
eval' (Val' x) = x
eval' (Add' x y) = eval' x + eval' y

eval'' :: Expr' -> String
eval'' (Val' x) = show x
eval'' (Add' x y) = "(" ++ eval'' x ++ " + " ++ eval'' y ++ ")"

eg' :: Int
eg' = eval' $ Add' (Val' 19) (Val' 23)

eg'' :: String
eg'' = eval'' $ Add' (Val' 19) (Val' 23)

-- unintuitive but "free" algebra
data Expr f = In (f (Expr f))

data Val e = Val Int

data Add e = Add e e

data (f :+: g) e = Inl (f e) | Inr (g e)

infixr 8 :+:

-- with the above, we can have expressions of the following
eg :: Expr (Val :+: Add)
eg = In (Inr (Add (In (Inl (Val 19)))
                  (In (Inl (Val 23)))
             )
        )

-- how do we evaluate the above expression?
instance Functor Val where
  fmap _ (Val x) = Val x  -- fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e e') = Add (f e) (f e')

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)

fold :: Functor f => (f a -> a) -> Expr f -> a
fold f (In t) = f (fmap (fold f) t)

-- define class of semantic domains
class Functor f => Eval f where
  evalAlg :: f Int -> Int

instance Eval Val where
  evalAlg (Val x) = x

instance Eval Add where
  evalAlg (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlg (Inl x) = evalAlg x
  evalAlg (Inr y) = evalAlg y

eval :: Eval f => Expr f -> Int
eval expr = fold evalAlg expr

-- we can now evaluate an expression in some semantic domain,
-- let's make it ergonimic
class (Functor sub, Functor sup) => sub < sup where
  inj :: sub a -> sup a

instance Functor f => f < f where
  inj = id

instance (Functor f, Functor g) => f < (f :+: g) where
  inj = Inl

instance {-# overlappable #-} (Functor f, Functor g, Functor h, f < g) => f < (h :+: g) where
  inj = Inr . inj

-- with the above machinery in place
inject :: (g < f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val < f) => Int -> Expr f
val x = inject (Val x)

infixl 6 `add`

add :: (Add < f) => Expr f -> Expr f -> Expr f
add x y = inject (Add x y)

-- with the above we can now define mul in a separate file
data Mul x = Mul x x

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlg (Mul x y) = x * y

infixl 7 `mul`

mul :: (Mul < f ) => Expr f -> Expr f -> Expr f
mul x y = inject (Mul x y)

-- free monad
data Term f a = Pure a | Impure (f (Term f a))

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
instance Functor f => Functor (Term f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Impure t) = Impure $ fmap (fmap f) t

--
instance Functor f => Applicative (Term f) where
  pure = Pure
  (Pure f) <*> t = fmap f t
  (Impure f) <*> t = Impure $ fmap (<*> t) f

instance Functor f => Monad (Term f) where
  return = Pure
  (Pure x) >>= f = f x
  (Impure t) >>= f = Impure $ fmap (>>= f) t

inject' :: (g < f ) => g (Term f a) -> Term f a
inject' = Impure . inj

-- with the above
data Tty a = GetChar (Char -> a) | PutChar Char a

instance Functor Tty where
  fmap f (GetChar k) = GetChar $ f . k
  fmap f (PutChar c k) = PutChar c (f k)

class Functor f => Exec f where
  execAlg :: f (IO a) -> IO a

instance (Exec f, Exec g) => Exec (f :+: g) where
  execAlg (Inl x) = execAlg x
  execAlg (Inr y) = execAlg y

instance Exec Tty where
  execAlg (GetChar k) = Prelude.getChar >>= k
  execAlg (PutChar c k) = Prelude.putChar c >> k

fold' :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
fold' p _ (Pure x) = p x
fold' p imp (Impure t) = imp (fmap (fold' p imp) t)

exec :: Exec f => Term f a -> IO a
exec = fold' return execAlg

getChar' :: (Tty < f) => Term f Char
getChar' = inject' (GetChar Pure)

putChar' :: (Tty < f) => Char -> Term f ()
putChar' c = inject' (PutChar c (Pure ()))

echo :: (Tty < f) => Term f ()
echo = forever $ getChar' >>= putChar'
