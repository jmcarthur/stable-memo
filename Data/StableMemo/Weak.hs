{-# LANGUAGE BangPatterns #-}
module Data.StableMemo.Weak (memo, memo2, memo3) where

import Data.Proxy

import System.Mem.Weak (Weak)

import qualified Data.StableMemo.Internal as Internal

memo :: (a -> b) -> (a -> b)
{-# NOINLINE memo #-}
memo = Internal.memo (Proxy :: Proxy Weak)

memo2 :: (a -> b -> c) -> (a -> b -> c)
memo2 f = memo . memo f

memo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f = memo . memo2 f
