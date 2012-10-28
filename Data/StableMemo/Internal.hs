{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
module Data.StableMemo.Internal (Ref (..), Strong (..), memo) where

import Data.Proxy
import System.Mem.StableName

import Data.HashTable.IO (BasicHashTable)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (Weak)

import qualified Data.HashTable.IO as HashTable
import qualified System.Mem.Weak as Weak

type SNMap a b = BasicHashTable (StableName a) b
type MemoTable ref a b = SNMap a (ref b)

class Ref ref where
  mkRef    :: a -> b -> IO () -> IO (ref b)
  deRef    :: ref a -> IO (Maybe a)
  finalize :: ref a -> IO ()

instance Ref Weak where
  mkRef x y = Weak.mkWeak x y . Just
  deRef = Weak.deRefWeak
  finalize = Weak.finalize

data Strong a = Strong a !(Weak a)

instance Ref Strong where
  mkRef _ y final = do
    weak <- Weak.mkWeakPtr y $ Just final
    return $ Strong y weak
  deRef (Strong x _) = return $ Just x
  finalize (Strong _ weak) = Weak.finalize weak

finalizer :: StableName a -> Weak (MemoTable ref a b) -> IO ()
finalizer sn weakTbl = do
  r <- Weak.deRefWeak weakTbl
  case r of
    Nothing -> return ()
    Just tbl -> HashTable.delete tbl sn

memo' :: Ref ref => Proxy ref -> (a -> b) -> MemoTable ref a b -> Weak (MemoTable ref a b) -> (a -> b)
memo' _ f tbl weakTbl !x = unsafePerformIO $ do
  sn <- makeStableName x
  lkp <- HashTable.lookup tbl sn
  case lkp of
    Nothing -> notFound sn
    Just w -> do
      maybeVal <- deRef w
      case maybeVal of
        Nothing -> notFound sn
        Just val -> return val
  where notFound sn = do
          let y = f x
          weak <- mkRef x y $ finalizer sn weakTbl
          HashTable.insert tbl sn weak
          return y

tableFinalizer :: Ref ref => MemoTable ref a b -> IO ()
tableFinalizer = HashTable.mapM_ $ finalize . snd

memo :: Ref ref => Proxy (ref :: * -> *) -> (a -> b) -> (a -> b)
memo p f =
  let (tbl, weak) = unsafePerformIO $ do
        tbl' <- HashTable.new
        weak' <- Weak.mkWeakPtr tbl . Just $ tableFinalizer tbl
        return (tbl', weak')
  in memo' p f tbl weak
