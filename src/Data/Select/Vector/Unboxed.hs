-- |
-- Module      : Data.Select.Vector.Unboxed
-- Description : Quickselect algorithm on unboxed vectors.
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides an implementation of quickselect on unboxed
-- vectors. It uses a median-of-medians based implementation, which
-- guarantees \(\mathcal{O}(n)\) worst-case time.
module Data.Select.Vector.Unboxed
  (selectBy
  ,select)
  where

import           Data.Vector.Unboxed                (Vector,Unbox)
import qualified Data.Vector.Unboxed                as Vector
import qualified Data.Vector.Unboxed.Mutable        as MVector

import           Control.Monad.ST

import qualified Data.Select.Vector.Mutable.Unboxed as M

-- | \(\mathcal{O}(n)\). Find the nth item, ordered by the supplied
-- relation.
--
-- prop> i >= 0 && i < Vector.length xs ==> sort xs Vector.! i === selectBy (<=) i (xs :: Vector Int)
selectBy
    :: Unbox a
    => (a -> a -> Bool) -> Int -> Vector a -> a
selectBy _ i xs
  | i < 0 || i >= Vector.length xs =
      error "Data.Select.Vector.Unboxed.selectBy: index out of bounds."
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
-- 3.0
select
    :: (Unbox a, Ord a)
    => Int -> Vector a -> a
select = selectBy (<=)
{-# INLINE select #-}

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import qualified Data.List as List
-- >>> import Test.QuickCheck
-- >>> :{
-- instance (Arbitrary a, Unbox a) => Arbitrary (Vector a) where
--   arbitrary = fmap Vector.fromList arbitrary
--   shrink = map Vector.fromList . shrink . Vector.toList
-- sort = Vector.fromList . List.sort . Vector.toList
-- :}
