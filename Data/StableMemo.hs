{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Data.StableMemo
Copyright   : (c) 2012, 2014 Jake McArthur
License     : MIT
Maintainer  : Jake McArthur <Jake.McArthur@gmail.com>
Stability   : provisional
Portability : GHC

This module provides a small set of functions for memoization.
Whereas most memo combinators memoize based on equality, these do
it based on whether the exact same argument has been passed to
the function before (that is, is the same argument in memory).

* They only evaluate keys to WHNF.

* Relative to value-base memoization, this can be more suitable
  for recursive functions over graphs with cycles.

* These don't retain the keys they have seen so far, which allows
  mappings to be garbage collected if they will no longer be
  used. Finalizers are put in place to remove the corresponding
  entries from the memo table if this happens.

* "Data.StableMemo.Weak" provides an alternative set of
  combinators that also avoid retaining the results of the
  function, only reusing results if they have not yet been
  garbage collected.

* There is no type class constraint on the function's argument.

These will not work for arguments which happen to have the same
value but are not the same heap object. This rules out many
candidates for memoization, such as the most common example, the
naive Fibonacci implementation whose domain is machine Ints; it
can still be made to work for some domains, though, such as the
lazy naturals.

> data Nat = Succ Nat | Zero
>
> fib :: Nat -> Integer
> fib = memo fib'
>   where fib' Zero                = 0
>         fib' (Succ Zero)         = 1
>         fib' (Succ n1@(Succ n2)) = fib n1 + fib n2

 Below is an implementation of map that preserves sharing of the
 spine for cyclic lists. It should even be safe to use this on
 arbitrarily long, acyclic lists since as long as the garbage
 collector is chasing you, the size of the memo table should stay
 under control, too.

> map :: (a -> b) -> [a] -> [b]
> map f = go
>   where go = memo map'
>         map' []     = []
>         map' (x:xs) = f x : go xs
-}

module Data.StableMemo (memo, memo2, memo3, memoPoly) where

import Control.Applicative
import Data.Proxy

import qualified Data.StableMemo.Internal as Internal

-- | Memoize a function with support for a certain form of polymorphic
-- recursion.
memoPoly :: (forall a. f a -> g a) -> f b -> g b
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
