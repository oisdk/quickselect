{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

-- |
-- Module      : Data.Select.Vector.Mutable
-- Description : Quickselect internals on mutable, boxed vectors.
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- Quickselect internals on mutable, boxed vectors.
module Data.Select.Vector.Mutable
  (select
  ,partition
  ,pivot)
  where

import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as Vector

import           Data.Median.Small
import           Data.Select.Small

import           Control.Applicative
import           Control.Monad.ST

import           Data.Bits

ilg :: Int -> Int
#if MIN_VERSION_base(4,7,0)
ilg !x = 2 * finiteBitSize x - 1 - countLeadingZeros x
#else
ilg !m = 2 * loop m 0
  where
    loop 0 !k = k - 1
    loop n !k = loop (n `shiftR` 1) (k+1)
#endif
{-# INLINE ilg #-}

-- | @'select' ('<=') xs lb ub n@ returns the 'n'th item in the
-- indices in the inclusive range ['lb','ub'].
select :: (a -> a -> Bool) -> MVector s a -> Int -> Int -> Int -> ST s Int
select lte !xs !l !r = selectGo lte xs (ilg (r-l)) l r
{-# INLINE select #-}

selectGo :: (a -> a -> Bool) -> MVector s a -> Int -> Int -> Int -> Int -> ST s Int
selectGo lte !xs 0 !l !r !n = selectWorstCase lte xs l r n
selectGo lte !xs !d !l !r !n =
    case r - l of
        0 -> pure l
        1 ->
            (l +) <$>
            liftA2
                (select2 lte (n - l))
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
        2 ->
            (l +) <$>
            liftA3
                (select3 lte (n - l))
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
                (Vector.unsafeRead xs (l + 2))
        3 ->
            (l +) <$>
            liftA4
                (select4 lte (n - l))
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
                (Vector.unsafeRead xs (l + 2))
                (Vector.unsafeRead xs (l + 3))
        4 ->
            (l +) <$>
            liftA5
                (select5 lte (n - l))
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
                (Vector.unsafeRead xs (l + 2))
                (Vector.unsafeRead xs (l + 3))
                (Vector.unsafeRead xs (l + 4))
        s -> do
            i <-
                partition lte xs l r =<<
                ((l +) <$>
                 liftA3
                     (median3 lte)
                     (Vector.unsafeRead xs l)
                     (Vector.unsafeRead xs (l + (s `div` 2)))
                     (Vector.unsafeRead xs r))
            case compare n i of
                EQ -> pure n
                LT -> selectGo lte xs (d - 1) l (i - 1) n
                GT -> selectGo lte xs (d - 1) (i + 1) r n
{-# INLINABLE selectGo #-}

selectWorstCase :: (a -> a -> Bool) -> MVector s a -> Int -> Int -> Int -> ST s Int
selectWorstCase lte !xs !l !r !n =
    case r - l of
        0 -> pure l
        1 ->
            (l +) <$>
            liftA2
                (select2 lte (n-l))
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
        2 ->
            (l +) <$>
            liftA3
                (select3 lte (n-l))
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
                (Vector.unsafeRead xs (l + 2))
        3 ->
            (l +) <$>
            liftA4
                (select4 lte (n-l))
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
                (Vector.unsafeRead xs (l + 2))
                (Vector.unsafeRead xs (l + 3))
        4 ->
            (l +) <$>
            liftA5
                (select5 lte (n-l))
                (Vector.unsafeRead xs l)
                (Vector.unsafeRead xs (l + 1))
                (Vector.unsafeRead xs (l + 2))
                (Vector.unsafeRead xs (l + 3))
                (Vector.unsafeRead xs (l + 4))
        _ -> do
                i <- partition lte xs l r =<< pivot lte xs l r
                case compare n i of
                    EQ -> pure n
                    LT -> selectWorstCase lte xs l (i - 1) n
                    GT -> selectWorstCase lte xs (i + 1) r n
{-# INLINABLE selectWorstCase #-}

-- | @'partition' ('<=') xs lb ub n@ partitions the section of the
-- list defined by the inclusive slice ['lb','ub'] around the element
-- at 'n'.
partition :: (a -> a -> Bool) -> MVector s a -> Int -> Int -> Int -> ST s Int
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
pivot :: (a -> a -> Bool) -> MVector s a -> Int -> Int -> ST s Int
pivot lte !xs !l !r = go l
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
                Vector.unsafeSwap xs (i + m) j
                end
            3 -> do
                m <-
                    liftA4
                        (median4 lte)
                        (Vector.unsafeRead xs i)
                        (Vector.unsafeRead xs (i + 1))
                        (Vector.unsafeRead xs (i + 2))
                        (Vector.unsafeRead xs (i + 3))
                Vector.unsafeSwap xs (i + m) j
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
                Vector.unsafeSwap xs (i + m) j
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
                Vector.unsafeSwap xs (i + m) j
                go (i + 5)
      where
        !j = l + ((i - l) `div` 5)
    end = selectWorstCase lte xs l (l + ((r - l) `div` 5)) (l + ((r - l) `div` 10))
{-# INLINABLE pivot #-}
