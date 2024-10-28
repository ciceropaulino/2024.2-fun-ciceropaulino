module Nat where

import Prelude hiding (Num (..))

-- Naturals definition
data Nat = O | S Nat deriving (Eq, Show)

-- Question 1. Define (without consultation) addition, multiplication, and exponentiation (in Nats).
(+) :: Nat -> Nat -> Nat
n + O = n
n + (S m) = S (n + m)
