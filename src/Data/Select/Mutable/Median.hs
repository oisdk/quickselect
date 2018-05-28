{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Select.Mutable.Median
-- Description : median-of-medians internals on mutable, boxed vectors.
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- Median-of-medians internals on mutable, boxed vectors.
module Data.Select.Mutable.Median
  (select)
  where

import           Data.Vector.Generic.Mutable          (MVector)
import qualified Data.Vector.Generic.Mutable          as Vector

import           Data.Vector.Mutable.Partition

import           Data.Median.Optimal
import           Data.Select.Optimal

import           Control.Monad.ST
import           Control.Applicative.LiftMany

-- | @'select' ('<=') xs lb ub n@ returns the 'n'th item in the
-- indices in the inclusive range ['lb','ub'].
select
    :: MVector v a
    => (a -> a -> Bool) -> v s a -> Int -> Int -> Int -> ST s Int
select lte !xs !l' !r' !n' = go l' r' n'
  where
    go l r n =
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
            _ -> do
                i <- partition lte xs l r =<< pivot l
                case compare n i of
                    EQ -> pure n
                    LT -> go l (i - 1) n
                    GT -> go (i + 1) r n
      where
        pivot = pgo
          where
            pgo !i =
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
                        pgo (i + 5)
              where
                !j = l + ((i - l) `div` 5)
            end = go l (l + ((r - l) `div` 5)) (l + ((r - l) `div` 10))
{-# INLINE select #-}
