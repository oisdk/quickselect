-- |
-- Module      : Data.Select.Vector
-- Description : Introselect algorithm on boxed vectors.
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides an implementation of introselect on boxed
-- vectors. It begins as quickselect, but if it discovers it's in a
-- pathological case, it switches to a median-of-medians
-- implementation. This guarantees \(\mathcal{O}(n)\) worst-case time.
module Data.Select.Vector
  (selectBy
  ,select)
  where

import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector
import qualified Data.Vector.Mutable        as MVector

import           Control.Monad.ST

import qualified Data.Select.Vector.Mutable as M

-- | \(\mathcal{O}(n)\). Find the nth item, ordered by the supplied
-- relation.
--
-- prop> i >= 0 && i < length xs ==> sort xs !! i === selectBy (<=) i (Vector.fromList xs)
selectBy :: (a -> a -> Bool) -> Int -> Vector a -> a
selectBy _ i xs
  | i < 0 || i >= Vector.length xs =
      error "Data.Select.Vector.selectBy: index out of bounds."
selectBy lte i xs = runST $ do
    ys <- Vector.thaw xs
    j <- M.select lte ys 0 (Vector.length xs - 1) i
    MVector.unsafeRead ys j
{-# INLINE selectBy #-}

-- | \(\mathcal{O}(n)\). Find the nth smallest item in the vector.
--
-- >>> select 4 (Vector.fromList "this is an example")
-- 'a'
--
-- >>> select 3 (Vector.fromList [0,1,4,2,3,5,6])
-- 3
select :: Ord a => Int -> Vector a -> a
select = selectBy (<=)
{-# INLINE select #-}

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Data.List (sort)
-- >>> import Test.QuickCheck
