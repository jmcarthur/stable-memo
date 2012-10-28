{-# LANGUAGE BangPatterns #-}
module Data.StableMemo (memo, memo2, memo3) where

import Data.Proxy

import qualified Data.StableMemo.Internal as Internal

-- | Memoize a unary function.
memo :: (a -> b) -> (a -> b)
{-# NOINLINE memo #-}
memo = Internal.memo (Proxy :: Proxy Internal.Strong)

-- | Curried memoization to share partial evaluation
memo2 :: (a -> b -> c) -> (a -> b -> c)
memo2 f = memo (memo . f)

-- | Curried memoization to share partial evaluation
memo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f = memo (memo2 . f)
