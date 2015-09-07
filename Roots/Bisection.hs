--
-- Author: Benjamin Schulz
-- License: BSD3
--

{-# LANGUAGE   TypeFamilies   #-}

module Numerical.Roots.Bisection where

import Numerical.Roots.Basic
import Numerical.Refinable
import Numerical.Iteration (untilCountMaybe)
import Control.FPipe
import Control.Monad (liftM)

intervalSpan (a,b) = let (xL,xH) = if a<b then (a,b) else (b,a) in xH - xL


-- Bisection --------------------------------------------------------------

data BisectSearch = BisectSearch {
    function  :: Double -> Double
  , bracket   :: !(Double,Double)
  , tolerance :: Double }

instance Refinable BisectSearch where
  type RTy BisectSearch = (Double,Double)
  type QTy BisectSearch = ()
  bestRefinement = bracket
  isRefined r = intervalSpan (bracket r) < tolerance r
  nextStep r = let f = function r in
               bisectBy f hasRootMonotonic (bracket r) $>
               liftM (\p -> r { bracket=p })
  quality = undefined

bisectToRootBracket f p eps = refine (BisectSearch f p eps) 100

bisectToRoot f p eps = bisectToRootBracket f p eps $> liftM (\(a,b)->0.5*(a+b))


bisectBy :: (Fractional u) =>
            (u -> v)         -- ^ Function
         -> ((v,v) -> Bool)  -- ^ Predicate that the target interval has to satisfy
         -> (u,u)            -- ^ Interval to bisect
         -> Maybe (u,u)
bisectBy f p (a,b) = let m = center (a,b) in
                     if p (f a,f m) then Just (a,m)
                                    else if p (f m,f b) then Just (m,b)
                                                        else Nothing

bisectInwardUntil :: (Fractional u, Ord u) =>
                     (u -> v) -> ((v,v) -> Bool) -> (u,u) -> u -> Maybe (u,u)
bisectInwardUntil f p (a,b) eps = 
    untilCountMaybe isIntervalSmallEnough (bisectBy f p) (a,b) 1000 $> liftM dropCount
     where isIntervalSmallEnough (a,b) = abs (b - a) <= eps
           dropCount (r,(a,b)) = (a,b)

bisectToRoot' :: (Fractional u, Fractional v, Ord u, Ord v) =>
                (u -> v) -> (u,u) -> u -> Maybe (u,u)
bisectToRoot' f (a,b) eps = bisectInwardUntil f hasRootMonotonic (a,b) eps

bisectToRootStep f (a,b) = bisectBy f hasRootMonotonic (a,b)

firstOrderApproach :: (RealFloat a) => (a -> b) -> (b -> a) -> (a, b) -> Maybe a
firstOrderApproach f ef (x0, y0) =
    let small = 1.0e-5
        xLow = x0 * 0.99 - small
        xHigh = x0 * 1.01 + small
        yLow = f xLow
        yHigh = f xHigh
        eHigh = ef yHigh
        eLow  = ef yLow
        e0 = ef y0
        eSlope = (eHigh - eLow) / (xHigh - xLow)
        est = x0 - e0 / eSlope in  -- Estimate of where the error is zero.
    if badGuess est then Nothing else Just est
        where badGuess x = isInfinite x || isNaN x

