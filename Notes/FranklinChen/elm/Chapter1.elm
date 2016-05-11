module Chapter1 where

{-| Warning: this shadows the implicitly imported `abs`.
-}
abs x =
  case compare x 0 of
    GT -> x
    EQ -> 0
    LT -> -x


square x = x*x

average x y = (x+y)/2

{-| Newton's method.
  TODO Prefer to write without explicit recursion.
-}
sqrtIter guess x =
  if goodEnough guess x
  then guess
  else sqrtIter (improve guess x) x

improve guess x = average guess (x/guess)

goodEnough guess x = abs (square guess - x) < 0.001

ourSqrt x = sqrtIter 1.0 x

{-| With information hiding.
  My notes mention that hiding should be done with modules instead.
-}
ourSqrt2 x =
  let goodEnough guess x = abs (square guess - x) < 0.001
      improve guess x = average guess (x/guess)
      sqrtIter guess x =
        if goodEnough guess x
        then guess
        else sqrtIter (improve guess x) x
  in sqrtIter 1.0 x


{-| Counting change.
-}
countChange amount =
  cc amount 5

-- Breaking news: Elm 1.6 will be removing its cond-equivalent
-- multiway conditional because of usability issues, so I'm using
-- a standard if/else instead.
cc amount kindsOfCoins =
  if amount == 0
  then 1
  else if amount < 0 || kindsOfCoins == 0
  then 0
  else cc amount (kindsOfCoins-1)
     + cc (amount - firstDenomination kindsOfCoins) kindsOfCoins

firstDenomination kindsOfCoins =
  case kindsOfCoins of
    1 -> 1
    2 -> 5
    3 -> 10
    4 -> 25
    5 -> 50

-- 292
result_100 = countChange 100
