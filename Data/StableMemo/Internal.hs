{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
module Data.StableMemo.Internal (Ref (..), Strong (..), (-->) (), memo) where

import Data.Proxy
import System.Mem.StableName

import Data.HashTable.IO (BasicHashTable)
import GHC.Prim (Any)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (Weak)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.HashTable.IO as HashTable
import qualified System.Mem.Weak as Weak

newtype (f <<< g) a = O { unO :: f (g a) }

-- Invariant: The type parameters for a key and its corresponding
-- value are the same.
type SNMap f g = BasicHashTable (StableName (f Any)) (g Any)

type MemoTable ref f g = SNMap f (ref <<< g)

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

finalizer :: StableName (f Any) -> Weak (MemoTable ref f g) -> IO ()
finalizer sn weakTbl = do
  r <- Weak.deRefWeak weakTbl
  case r of
    Nothing -> return ()
    Just tbl -> HashTable.delete tbl sn

unsafeToAny :: f a -> f Any
unsafeToAny = unsafeCoerce

unsafeFromAny :: f Any -> f a
unsafeFromAny = unsafeCoerce

-- | Polymorphic memoizable function
type f --> g = forall a. f a -> g a

memo' :: Ref ref =>
         Proxy ref -> (f --> g) -> MemoTable ref f g ->
         Weak (MemoTable ref f g) -> (f --> g)
memo' _ f tbl weakTbl !x = unsafePerformIO $ do
  sn <- makeStableName $ unsafeToAny x
  lkp <- HashTable.lookup tbl sn
  case lkp of
    Nothing -> notFound sn
    Just (O w) -> do
      maybeVal <- deRef w
      case maybeVal of
        Nothing -> notFound sn
        Just val -> return $ unsafeFromAny val
  where notFound sn = do
          let y = f x
          weak <- mkRef x (unsafeToAny y) $ finalizer sn weakTbl
          HashTable.insert tbl sn $ O weak
          return y

tableFinalizer :: Ref ref => MemoTable ref f g -> IO ()
tableFinalizer = HashTable.mapM_ $ finalize . unO . snd

memo :: Ref ref => Proxy (ref :: * -> *) -> (f --> g) -> (f --> g)
memo p f =
  let (tbl, weak) = unsafePerformIO $ do
        tbl' <- HashTable.new
        weak' <- Weak.mkWeakPtr tbl . Just $ tableFinalizer tbl
        return (tbl', weak')
  in memo' p f tbl weak
