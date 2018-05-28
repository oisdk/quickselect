module Data.Select.Vector where

import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector
import qualified Data.Vector.Mutable        as MVector

import           Control.Monad.ST

import qualified Data.Select.Vector.Mutable as M

selectBy :: (a -> a -> Bool) -> Int -> Vector a -> a
selectBy _ i xs
  | i < 0 || i >= Vector.length xs =
      error "Data.Select.Vector.selectBy: index out of bounds."
selectBy lte i xs = runST $ do
    ys <- Vector.thaw xs
    j <- M.select lte ys 0 (Vector.length xs - 1) i
    MVector.unsafeRead ys j
{-# INLINE selectBy #-}

select :: Ord a => Int -> Vector a -> a
select = selectBy (<=)
{-# INLINE select #-}
