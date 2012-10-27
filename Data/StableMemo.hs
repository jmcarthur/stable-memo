{-# LANGUAGE BangPatterns #-}
module Data.StableMemo (memo, memo2, memo3) where

import Data.Proxy

import System.Mem.Weak (Weak)

import qualified Data.StableMemo.Internal as Internal
import qualified System.Mem.Weak as Weak

data Strong a = Strong a !(Weak a)

instance Internal.Ref Strong where
  mkRef _ y final = do
    weak <- Weak.mkWeakPtr y $ Just final
    return $ Strong y weak
  deRef (Strong x _) = return $ Just x
  finalize (Strong _ weak) = Weak.finalize weak

-- | Memoize a unary function.
memo :: (a -> b) -> (a -> b)
{-# NOINLINE memo #-}
memo = Internal.memo (Proxy :: Proxy Strong)

-- | Curried memoization to share partial evaluation
memo2 :: (a -> b -> c) -> (a -> b -> c)
memo2 f = memo . memo f

-- | Curried memoization to share partial evaluation
memo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f = memo . memo2 f
