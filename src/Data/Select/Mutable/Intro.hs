{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

-- |
-- Module      : Data.Select.Mutable.Intro
-- Description : Introselect internals on mutable, generic vectors.
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- Introselect internals on mutable, generic vectors.
module Data.Select.Mutable.Intro
  (select)
  where

import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as Vector

import qualified Data.Select.Mutable.Median as WorstCase

import           Data.Vector.Mutable.Partition

import           Data.Median.Optimal
import           Data.Select.Optimal

import           Control.Monad.LiftMany.Strict
import           Control.Monad.ST

import           Data.Bits

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (pure)
#endif

-- | @'select' ('<=') xs lb ub n@ returns the 'n'th item in the
-- indices in the inclusive range ['lb','ub'].
select
    :: MVector v a
    => (a -> a -> Bool) -> v s a -> Int -> Int -> Int -> ST s Int
select lte !xs !l' !r' !n = go (ilg (r' - l')) l' r'
  where
#if MIN_VERSION_base(4,8,0)
    ilg !x = 2 * finiteBitSize x - 1 - countLeadingZeros x
#else
    ilg !m = 2 * loop m (0 :: Int)
      where
        loop 0 !k = k - 1
        loop n' !k = loop (n' `shiftR` 1) (k+1)
#endif
    {-# INLINE ilg #-}

    {-# INLINABLE go #-}
    go 0 !l !r = WorstCase.select lte xs l r n
    go !d !l !r =
        case r - l of
            0 -> pure l
            1 -> do
                !i <-
                    liftM2
                        (select2 lte (n - l))
                        (Vector.unsafeRead xs l)
                        (Vector.unsafeRead xs (l + 1))
                pure $! i + l
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
                                     "Data.Select.Mutable.Intro.select: bug!")
                case compare n i of
                    EQ -> pure n
                    LT -> go (d - 1) l (i - 1)
                    GT -> go (d - 1) (i + 1) r
{-# INLINE select #-}
