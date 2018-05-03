{-# language FlexibleInstances, ScopedTypeVariables #-}
module BirdTree where

import Data.Ratio
import Prelude hiding (zip, mirror)

data Tree a = Node { root :: a, left :: Tree a, right :: Tree a }
 deriving Show

instance Functor Tree where
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative Tree where
  pure a = t where t = Node a t t
  t <*> u = Node ((root t) (root u)) (left t <*> left u) (right t <*> right u)

instance {-# overlappable #-} (Applicative a, Num b) => Num (a b) where
  (+) = zip' (+)
  (-) = zip' (-)
  (*) = zip' (*)
  abs = map' abs
  signum = map' signum
  negate = map' negate
  fromInteger = pure . fromInteger

map' :: (Applicative a, Num b) => (b -> b) -> a b -> a b
map' f t = pure f <*> t

zip' :: (Applicative a, Num b) => (b -> b -> b) -> a b -> a b -> a b
zip' g t u = pure g <*> t <*> u

inverse :: Tree Rational -> Tree Rational
inverse (Node a l r) = Node (1 / a) (inverse l) (inverse r)

mirror :: Tree Rational -> Tree Rational
mirror (Node a l r) = Node a (mirror r) (mirror l)

-- inverse bird = 1 / bird
-- bird = Node 1 l r where
--  l = 1 / (bird + 1)
--  r = (1 / bird) + 1
bird :: Tree Rational
bird = Node 1 l r
  where l = inverse (bird + 1)
        r = (inverse bird) + 1

bird' :: Tree Rational
bird' = mirror bird

fibs = map numerator $ left' bird
nats = 0 : (map numerator $ nat' R bird)
goldenRatios = right' bird

-- (1 + √5) / 2
goldenRatio :: Int -> Double
goldenRatio i = fromRational $ (right' bird) !! i

bfs :: Rational -> Tree Rational -> Tree Rational
bfs r t = bfs' r ts
  where bfs' r ts = if r == (root . head) ts
                    then head ts
                    else bfs' r (tail ts)
        ts = flatten t

breadth :: Tree a -> [a]
breadth = (map root) . flatten

flatten :: Tree a -> [Tree a]
flatten tree = bf [tree]
  where bf xs = xs ++ bf (concat $ map trees xs)
          where trees (Node _ l r) = [l, r]

left' :: Tree a -> [a]
left' t = (root t) : (left' $ left t)

right' :: Tree a -> [a]
right' t = (root t) : (right' $ right t)

data Dir = L | R

nat' :: Dir -> Tree a -> [a]
nat' L t = (root t) : (nat' R (left t))
nat' R t = (root t) : (nat' L (right t))
