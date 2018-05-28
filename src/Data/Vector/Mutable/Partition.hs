{-# LANGUAGE BangPatterns #-}

module Data.Vector.Mutable.Partition
  (partition)
  where

import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as Vector

import           Control.Monad.ST

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
