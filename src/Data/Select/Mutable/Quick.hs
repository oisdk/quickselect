{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

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

import           Control.Monad.LiftMany.Strict
import           Control.Monad.ST
import           Control.Monad (unless)

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (pure)
#endif

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
            1 -> do
                x <- Vector.unsafeRead xs l
                y <- Vector.unsafeRead xs (l+1)
                unless (lte x y) (Vector.unsafeWrite xs l y >> Vector.unsafeWrite xs (l+1) x)
                pure n
            2 -> do
                !i <-
                    liftM3
                        (select3 lte (n - l))
                        (Vector.unsafeRead xs l)
                        (Vector.unsafeRead xs (l + 1))
                        (Vector.unsafeRead xs (l + 2))
                pure $! i + l
            s -> do
                let !m = l + (s `div` 2)
                !p <-
                    liftM3
                        (median3 lte)
                        (Vector.unsafeRead xs l)
                        (Vector.unsafeRead xs m)
                        (Vector.unsafeRead xs r)
                !i <-
                    partition lte xs l r
                        (case p of
                             0 -> l
                             1 -> m
                             2 -> r
                             _ ->
#if MIN_VERSION_base(4,9,0)
                                 errorWithoutStackTrace
#else
                                 error
#endif
                                 "Data.Select.Mutable.Quick.select: bug!")
                case compare n i of
                    EQ -> pure n
                    LT -> go l (i - 1)
                    GT -> go (i + 1) r
    {-# INLINABLE go #-}
{-# INLINE select #-}
