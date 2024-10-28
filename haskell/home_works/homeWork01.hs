module Nat where

-- Naturals definition
data Nat = O | S Nat deriving (Eq, Show)
