--
-- Author: Benjamin Schulz
-- License: BSD3
--

module Numerical.Roots.Basic where

-- | When searching for roots, it's a good idea to maintain a bracket
-- around the suspected root. ReducibleInterval additionally allows us
-- to encode where in that interval we suspect the root to be.
data ReducibleInterval a = Low a
                         | High a
                         | LinearScale Double a
                         | Unknown a
                         deriving (Eq, Ord, Show)

reduceInterval (Low p) = fst p
reduceInterval (High p) = snd p
reduceInterval (LinearScale s (x, y)) = s * (y - x) + x
reduceInterval (Unknown p) = reduceInterval (LinearScale 0.5 p)

interval (Low p)  = p
interval (High p) = p
interval (LinearScale _ p) = p
interval (Unknown p) = p


-- Utilities --------------------------------------------------------------

center :: (Fractional t) => (t,t) -> t
center (a,b) = (a+b)/2.0

isReal x = not (isNaN x || isInfinite x)

-- | Returns wether the function f has a root on the interval a b, over which f
-- must be monotonic.
hasRootMonotonic :: (Fractional t, Ord t) => (t,t) -> Bool
hasRootMonotonic (y0, y1) = y0 * y1 <= 0.0

sign2 x = if x >= 0.0 then 1.0 else (-1.0)

absError :: (Num a) => a -> a -> a
absError t i = abs (t - i)

meetsTolerance :: (Ord a) => a -> a -> Bool
meetsTolerance tol delta = delta <= tol

funPair :: (a -> b) -> a -> (a, b)
funPair f x = (x, f x)

-- 
