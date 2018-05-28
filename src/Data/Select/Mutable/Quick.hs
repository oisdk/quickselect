{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Select.Mutable.Quick
-- Description : Quickselect internals on mutable, generic vectors.
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- Quickselect internals on mutable, generic vectors.
module Data.Select.Mutable.Quick
  (select)
  where

import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as Vector

import           Data.Vector.Mutable.Partition

import           Data.Median.Optimal
import           Data.Select.Optimal

import           Control.Applicative.LiftMany
import           Control.Monad.ST

-- | @'select' ('<=') xs lb ub n@ returns the 'n'th item in the
-- indices in the inclusive range ['lb','ub'].
select
    :: MVector v a
    => (a -> a -> Bool) -> v s a -> Int -> Int -> Int -> ST s Int
select lte !xs !l' !r' !n = go l' r'
  where
    go !l !r =
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
                  LT -> go l (i - 1)
                  GT -> go (i + 1) r
    {-# INLINABLE go #-}
{-# INLINE select #-}
