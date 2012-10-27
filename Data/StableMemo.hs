{-# LANGUAGE BangPatterns #-}
module Data.StableMemo (memo, memo2, memo3) where

import System.Mem.StableName
import System.Mem.Weak

import Data.HashTable.IO (BasicHashTable)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.HashTable.IO as HashTable

type SNMap a b = BasicHashTable (StableName a) b
type MemoTable a b = SNMap a (Weak b)

finalizer :: StableName a -> Weak (MemoTable a b) -> IO ()
finalizer sn weakTbl = do
  r <- deRefWeak weakTbl
  case r of
    Nothing -> return ()
    Just tbl -> HashTable.delete tbl sn

memo' :: (a -> b) -> MemoTable a b -> Weak (MemoTable a b) -> (a -> b)
memo' f tbl weakTbl !x = unsafePerformIO $ do
  sn <- makeStableName x
  lkp <- HashTable.lookup tbl sn
  case lkp of
    Nothing -> notFound sn
    Just w -> do
      maybeVal <- deRefWeak w
      case maybeVal of
        Nothing -> notFound sn
        Just val -> return val
  where notFound sn = do
          let y = f x
          weak <- mkWeak x y . Just $ finalizer sn weakTbl
          HashTable.insert tbl sn weak
          return y

tableFinalizer :: MemoTable a b -> IO ()
tableFinalizer = HashTable.mapM_ $ finalize . snd

memo :: (a -> b) -> (a -> b)
{-# NOINLINE memo #-}
memo f =
  let (tbl, weak) = unsafePerformIO $ do
        tbl' <- HashTable.new
        weak' <- mkWeakPtr tbl . Just $ tableFinalizer tbl
        return (tbl', weak')
  in memo' f tbl weak

memo2 :: (a -> b -> c) -> (a -> b -> c)
memo2 f = memo . memo f

memo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f = memo . memo2 f
