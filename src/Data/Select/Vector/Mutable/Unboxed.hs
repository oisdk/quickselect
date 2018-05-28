{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Select.Vector.Mutable.Unboxed
-- Description : Quickselect internals on mutable, unboxed vectors.
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- Quickselect internals on mutable, unboxed vectors.
module Data.Select.Vector.Mutable.Unboxed
  (select
  ,partition
  ,pivot)
  where

import           Data.Vector.Unboxed.Mutable (MVector,Unbox)
import qualified Data.Vector.Unboxed.Mutable as Vector

import           Data.Median.Small

import           Control.Applicative
import           Control.Monad.ST

-- | @'select' ('<=') xs lb ub n@ returns the 'n'th item in the
-- indices in the inclusive range ['lb','ub'].
select :: Unbox a => (a -> a -> Bool) -> MVector s a -> Int -> Int -> Int -> ST s Int
select lte !xs !l !r !n
  | l == r = pure l
  | otherwise = do
      i <- partition lte xs l r =<< pivot lte xs l r
      case compare n i of
          EQ -> pure n
          LT -> select lte xs l (i - 1) n
          GT -> select lte xs (i + 1) r n
{-# INLINABLE select #-}

-- | @'partition' ('<=') xs lb ub n@ partitions the section of the
-- list defined by the inclusive slice ['lb','ub'] around the element
-- at 'n'.
partition :: Unbox a => (a -> a -> Bool) -> MVector s a -> Int -> Int -> Int -> ST s Int
partition lte !xs !l !r !i = do
    x <- Vector.unsafeRead xs i
    Vector.unsafeSwap xs i r
    let go !s !j
          | j >= r = pure s
          | otherwise = do
              y <- Vector.unsafeRead xs j
              if lte y x
                  then do
                      Vector.unsafeSwap xs s j
                      go (s + 1) (j + 1)
                  else go s (j + 1)
    s <- go l l
    Vector.unsafeSwap xs r s
    pure s
{-# INLINE partition #-}

liftA4
  :: Applicative f =>
     (a1 -> b1 -> c -> a2 -> b2) -> f a1 -> f b1 -> f c -> f a2 -> f b2
liftA4 f w x y z = liftA3 f w x y <*> z
{-# INLINE liftA4 #-}

liftA5
  :: Applicative f =>
     (a1 -> b1 -> c -> a2 -> a3 -> b2)
     -> f a1 -> f b1 -> f c -> f a2 -> f a3 -> f b2
liftA5 f v w x y z = liftA3 f v w x <*> y <*> z
{-# INLINE liftA5 #-}

-- | Median-of-medians algorithm.
pivot
    :: Unbox a
    => (a -> a -> Bool) -> MVector s a -> Int -> Int -> ST s Int
pivot lte !xs !l !r =
    case r - l of
        0 -> pure l
        1 -> pure l
        2 ->
            (l +) <$>
            liftA3
                (median3 lte)
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
                (Vector.unsafeRead xs (l + 2))
        3 ->
            (l +) <$>
            liftA4
                (median4 lte)
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
                (Vector.unsafeRead xs (l + 2))
                (Vector.unsafeRead xs (l + 3))
        4 ->
            (l +) <$>
            liftA5
                (median5 lte)
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
                (Vector.unsafeRead xs (l + 2))
                (Vector.unsafeRead xs (l + 3))
                (Vector.unsafeRead xs (l + 4))
        _ -> go l
  where
    go !i =
        case r - i of
            0 -> do
                Vector.unsafeSwap xs i j
                end
            1 -> do
                Vector.unsafeSwap xs i j
                end
            2 -> do
                m <-
                    liftA3
                        (median3 lte)
                        (Vector.unsafeRead xs i)
                        (Vector.unsafeRead xs (i + 1))
                        (Vector.unsafeRead xs (i + 2))
                Vector.unsafeSwap xs (m + i) j
                end
            3 -> do
                m <-
                    liftA4
                        (median4 lte)
                        (Vector.unsafeRead xs i)
                        (Vector.unsafeRead xs (i + 1))
                        (Vector.unsafeRead xs (i + 2))
                        (Vector.unsafeRead xs (i + 3))
                Vector.unsafeSwap xs (m + i) j
                end
            4 -> do
                m <-
                    liftA5
                        (median5 lte)
                        (Vector.unsafeRead xs i)
                        (Vector.unsafeRead xs (i + 1))
                        (Vector.unsafeRead xs (i + 2))
                        (Vector.unsafeRead xs (i + 3))
                        (Vector.unsafeRead xs (i + 4))
                Vector.unsafeSwap xs (m + i) j
                end
            _ -> do
                m <-
                    liftA5
                        (median5 lte)
                        (Vector.unsafeRead xs i)
                        (Vector.unsafeRead xs (i + 1))
                        (Vector.unsafeRead xs (i + 2))
                        (Vector.unsafeRead xs (i + 3))
                        (Vector.unsafeRead xs (i + 4))
                Vector.unsafeSwap xs (m + i) j
                go (i + 5)
      where
        !j = l + ((i - l) `div` 5)
    end = select lte xs l (l + ((r - l) `div` 5)) (l + ((r - l) `div` 10))
{-# INLINABLE pivot #-}
