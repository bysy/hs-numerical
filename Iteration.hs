--
-- Author: Benjamin Schulz
-- License: BSD3
--

{-# LANGUAGE BangPatterns   #-}

module Numerical.Iteration where

import Control.FPipe

-- | This is a streamlined form of `until' that can fail in the function f.
untilMaybe :: (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
untilMaybe p f x = until p' f' (Just x) where 
    p' (Just x) = p x
    p' Nothing = True
    f' (Just x) = f x
    f' Nothing = Nothing

-- A form of `until' in the context of Maybe and iteration limiting.
untilCountMaybe :: (Num n, Ord n) => (a -> Bool) -> (a -> Maybe a) -> a -> n -> Maybe (n, a)
untilCountMaybe p f (!x) n = until p' f' (0, Just x) $> unwrap where
    p' !(i, Just x) = (p x)
    p' !(i, Nothing) = True
    f' !(i, Just x) = if i > n then (i, Nothing) else (i+1, f x)
    f' !(i, Nothing) = (i, Nothing)
    unwrap !(i, Just x) = Just (i, x)
    unwrap !(i, Nothing) = Nothing

