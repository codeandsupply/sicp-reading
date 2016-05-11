{-# LANGUAGE BangPatterns #-}

module Chapter1 where

-- | 1.5
p () = p ()

-- | Note that Haskell is normal-order by default.
test x y =
  if x == 0
  then 0
  else y

-- | Evaluation succeeds because p () is never evaluated.
--
-- 0
result_1_5_returns = test 0 (p ())

-- | In Haskell, you can also force evaluation manually with $!
result_1_5_never_returns1 = test 0 $! p ()

-- | We can force applicative-order with a strictness annotation on
-- the called function so that all callers are affected.
testStrict x !y =
  if x == 0
  then 0
  else y

-- | This never returns either.
result_1_5_never_returns2 = testStrict 0 (p ())
