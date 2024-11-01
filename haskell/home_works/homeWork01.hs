module Nat where

import Prelude hiding (Num (..), div, max, min, pred, (<), (<=), (>), (>=), (^))

-- Naturals definition
data Nat = O | S Nat deriving (Eq, Show)

-- [Question 1.] Define (without consultation) addition, multiplication, and exponentiation (in Nats).

-- Defining sum
(+) :: Nat -> Nat -> Nat
n + O = n
O + m = m
n + (S m) = S (n + m)

-- Defining Nat minus
nminus :: Nat -> Nat -> Nat
nminus n O = n
nminus O _ = O
nminus (S n) (S m) = nminus n m

-- Defining multiplication
(*) :: Nat -> Nat -> Nat
_ * O = O
n * (S m) = (n * m) + n

-- Defining exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
n ^ (S m) = n * (n ^ m)

-- [Question 3.] Define a double function that returns double its input.
double :: Nat -> Nat
double n = n * S (S o)

-- [Question 7.] Define the pred function.
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- [Question 8.] Define the functions:
-- [1.] Fact:
fact :: Nat -> Nat
fact O = S o
fact (S n) = S n * fact n

-- [2.] Fib:
fib :: Nat -> Nat
fib (S (S n)) = fib (S n) + fib n
fib n = n

-- [3.] Min:
min :: Nat -> Nat -> Nat
min (S n) (S m) = S (min n m)
min O O = O
min n O = O
min O m = O

-- [4.] Max:
max :: Nat -> Nat -> Nat
max (S n) (S m) = S (max n m)
max n O = n
max O m = m

-- [5.] Div:
-- div :: Nat -> Nat -> Nat
-- div m n = div (n - m) m
-- div n O = n
-- div O m = O

-- [Inequality operators]
--
-- [Syntactic Sugar]
o, so, sso, ssso, sssso, ssssso, sssssso, ssssssso, sssssssso, ssssssssso :: Nat
-- For 0
o = O
so = S o
sso = S so
ssso = S sso
sssso = S ssso
ssssso = S sssso
sssssso = S ssssso
ssssssso = S sssssso
sssssssso = S sssssssso
ssssssssso = S ssssssssso

-- to 10
