--
-- Author: Benjamin Schulz
-- License: BSD3
--

module Numerical.Integration.Relax where

import Data.Array

ixFlatten2 (i, j) = [0..(i*j)]
ixFlatten3 (i, j, k) = [0..(i*j*k)]

ixUnflatten2 j x = let i' = x `div` j
                       j' = x `mod` j in (i', j')

rel :: (Ix i, Num e) =>
  Array i e -> Array i e -> (Array i e -> i -> e) -> i -> Array i e
rel relaxFactors state nextStateF i = 
  let old = state!i
      new = nextStateF state i
      rf  = relaxFactors!i in 
  state//[(i, old - rf * old + rf * new)]

