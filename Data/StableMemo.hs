{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module Data.StableMemo (memo, memo2, memo3, (-->) (), memoPoly) where

import Control.Applicative
import Data.Proxy

import qualified Data.StableMemo.Internal as Internal

import Data.StableMemo.Internal ((-->) ())

-- | Memoize a function with support for polymorphic recursion.
memoPoly :: (f --> g) -> (f --> g)
{-# NOINLINE memoPoly #-}
memoPoly = Internal.memo (Proxy :: Proxy Internal.Strong)

-- | Memoize a unary function.
memo :: (a -> b) -> (a -> b)
memo f = getConst . memoPoly (Const . f . getConst) . Const

-- | Curried memoization to share partial evaluation
memo2 :: (a -> b -> c) -> (a -> b -> c)
memo2 f = memo (memo . f)

-- | Curried memoization to share partial evaluation
memo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f = memo (memo2 . f)
