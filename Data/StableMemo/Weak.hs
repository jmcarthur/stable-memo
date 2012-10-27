{-|
This module provides memo combinators with slightly different behavior
from those in "Data.StableMemo". The memo tables generated by these
combinators use weak pointers to store the values, so they not only do
not unnecessarily retain the arguments, but they also do not retain
the function outputs. This can be useful for memoized functions that
are expected to be around for a long time. If the result for an input
has already been computed and happens to still be in the heap, it will
be reused, otherwise it will be recomputed.
-}

{-# LANGUAGE BangPatterns #-}
module Data.StableMemo.Weak (memo, memo2, memo3) where

import Data.Proxy

import System.Mem.Weak (Weak)

import qualified Data.StableMemo.Internal as Internal

-- | Memoize a unary function.
memo :: (a -> b) -> (a -> b)
{-# NOINLINE memo #-}
memo = Internal.memo (Proxy :: Proxy Weak)

-- | Curried memoization to share partial evaluation
memo2 :: (a -> b -> c) -> (a -> b -> c)
memo2 f = memo . memo f

-- | Curried memoization to share partial evaluation
memo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f = memo . memo2 f
