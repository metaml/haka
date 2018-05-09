{-# language ScopedTypeVariables #-}
module Fix where

import Control.Monad.Fix
import Data.Fix

zeroes :: [Int]
zeroes = fix (\f -> 0:f)

gcd :: Integral a => a -> a -> a
gcd = fix f
  where f = \f' x y -> case (abs x, abs y) of
                         (x, 0) -> x
                         (x, y) -> f' y (x `rem` y)

fac :: Integer -> Integer
fac = fix f
  where f = \f' n -> if n == 0 then 1 else n * f' (n - 1)

fib :: [Integer]
fib = fix f
  where f = \xs -> 0 : scanl (+) 1 xs
