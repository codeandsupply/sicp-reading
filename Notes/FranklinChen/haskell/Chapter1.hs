module Chapter1 where

-- | Newton's method.
-- TODO Prefer to write without explicit recursion.
sqrtIter guess x
  if goodEnough guess x
  then guess
  else sqrtIter (improve guess x) x

improve guess x = average guess (x/guess)

average x y = (x+y)/2

goodEnough guess x = abs (square guess - x) < 0.001

ourSqrt x = sqrtIter 1.0 x

square x = x*x
