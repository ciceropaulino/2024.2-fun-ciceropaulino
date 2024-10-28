module Nat where

import Prelude hiding (Num (..), (^))

-- Naturals definition
data Nat = O | S Nat deriving (Eq, Show)

-- [Question 1.] Define (without consultation) addition, multiplication, and exponentiation (in Nats).

-- Defining sum
(+) :: Nat -> Nat -> Nat
n + O = n
n + (S m) = S (n + m)

-- Defining multiplication
(*) :: Nat -> Nat -> Nat
_ * O = O
n * (S m) = (n * m) + n

-- Defining exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
n ^ (S m) = n * (n ^ m)

-- [Syntactic Sugar]
o, so, sso, ssso, sssso, ssssso, sssssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso
sssso = S ssso
ssssso = S sssso
sssssso = S ssssso
