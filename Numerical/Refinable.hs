--
-- Author: Benjamin Schulz
-- License: BSD3
--

{-# LANGUAGE   TypeFamilies   #-}

module Numerical.Refinable where

import Numerical.Iteration (untilCountMaybe)
import Control.Monad
import Data.Maybe
import Control.FPipe

class Refinable s where
    type QTy s :: *
    type RTy s :: *
    bestRefinement :: s -> RTy s
    isRefined :: s -> Bool
    nextStep :: s -> Maybe s
    quality :: s -> QTy s

refineV :: (Refinable s, Integral i) => s -> i -> Maybe (i, s)
refineV s maxSteps = untilCountMaybe isRefined nextStep s maxSteps

refine :: (Refinable s, Integral i) => s -> i -> Maybe (RTy s)
refine s maxSteps = refineV s maxSteps $> liftM (bestRefinement . snd)

