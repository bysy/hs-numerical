--
-- Author: Benjamin Schulz
-- License: BSD3
--

{-# LANGUAGE   TypeFamilies   #-}

module Numerical.Roots.Ridder where

import Numerical.Roots.Basic
import Numerical.Refinable
import Control.FPipe
import Control.Monad (liftM)
import Debug.Trace (traceShow)

-- Ridder's method --------------------------------------------------------

data RidderSearch = RidderSearch {
                        function  :: Double -> Double
                      , bracket   :: !(ReducibleInterval (Double,Double))
                      , tolerance :: Double 
                      , delta     :: !Double  -- Represents bracket narrowing
                      }

instance Refinable RidderSearch where
  type RTy RidderSearch = Double
  type QTy RidderSearch = ()
  bestRefinement = reduceInterval . bracket
  isRefined r = abs (delta r) <= tolerance r
  nextStep r = ridderBracketAndDelta (function r) (interval . bracket $ r) 
               $> liftM (\(b,d) -> r { bracket=b, delta=d })
  quality = undefined

-- | Use Ridder's method to approximate the root of f on the interval x0, x1.
-- | The guess is guaranteed to be on (x0, x1) for a well-behaved f.
ridderApprox :: (Floating t, Ord t) => (t -> t) -> (t,t) -> t
ridderApprox f (xL,xH) =
  let (yL, yH) = (f xL, f xH)
      xm = center (xL,xH)
      ym = f xm in
  xm + (xm - xL) * (sign2 (yL - yH)) * ym / (sqrt (ym * ym - yL * yH))

ridderBracketAndDelta :: (RealFloat t, Show t) => 
                         (t -> t) -> (t, t) -> Maybe (ReducibleInterval (t, t), t)
ridderBracketAndDelta f (xL,xH) =
  let xEst = let x = (ridderApprox f (xL,xH)) in x `traceA` 
                                           ("Ridder inner loop -> " ++ (show x))
      yEst = f xEst in
  -- No (isReal yEst) check is necessary since checking if f is poorly-behaved
  -- is overly defensive. To see how to decide which of the boundaries to
  -- replace with the estimate, keep in mind that f x is required to not decrease
  -- on the interval (xL, xH). The second member of the pair, the delta value, 
  -- indicates the signed reduction of the bracket span.
  if not (isReal xEst) then Nothing 
                       else Just (if yEst < 0.0 then (Low  (xEst,xH), xL-xEst)
                                                else (High (xL,xEst), xH-xEst))
 where traceA x _ = x
 --where traceA = flip traceShow

ridderToRoot :: (Double -> Double) -> (Double, Double) -> Double -> Maybe Double
ridderToRoot f (a,b) eps =
  let (xL,xH) = if f a < 0.0 then (a,b) else (b,a)
      r = RidderSearch f (Low (xL,xH)) eps (1.0/0.0) in
  if not (hasRootMonotonic (f xL, f xH)) then Nothing
                                         else refine r 40

