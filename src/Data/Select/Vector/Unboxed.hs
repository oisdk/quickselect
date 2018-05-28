module Data.Select.Vector.Unboxed where

import           Data.Vector.Unboxed                (Vector,Unbox)
import qualified Data.Vector.Unboxed                as Vector
import qualified Data.Vector.Unboxed.Mutable        as MVector

import           Control.Monad.ST

import qualified Data.Select.Vector.Mutable.Unboxed as M

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

select
    :: (Unbox a, Ord a)
    => Int -> Vector a -> a
select = selectBy (<=)
{-# INLINE select #-}
