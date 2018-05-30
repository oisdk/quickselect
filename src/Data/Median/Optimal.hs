-- |
-- Module      : Data.Median.Small
-- Description : Optimal median-finding functions for small input.
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides optimal median-finding functions for small,
-- fixed-size inputs. Each function returns the (0-based) index of the
-- argument which is the median, according to the supplied relation.

module Data.Median.Optimal
  (median3
  ,median4
  ,median5)
  where

import GHC.Exts (inline)

-- | Find the median of 3 items, optimally.
--
-- >>> median3 (<=) 1 2 3
-- 1
-- >>> median3 (<=) 1 3 2
-- 2
-- >>> median3 (<=) 2 3 1
-- 0
--
-- prop> [x,y,z] !! median3 (<=) x y z === sort [x,y,z] !! 1
median3 :: (a -> a -> Bool) -> a -> a -> a -> Int
median3 lte a b c =
  if inline lte a b
    then
      if inline lte a c
        then
          if inline lte b c
            then 1
            else 2
        else 0
    else
      if inline lte b c
        then
          if inline lte a c
            then 0
            else 2
        else 1
{-# INLINE median3 #-}

-- | Find the median of 4 items, optimally.
--
-- >>> median4 (<=) 1 4 3 2
-- 2
-- >>> median4 (<=) 1 3 2 4
-- 1
-- >>> median4 (<=) 2 4 1 3
-- 3
-- >>> median4 (<=) 3 1 4 2
-- 0
--
-- prop> [w,x,y,z] !! median4 (<=) w x y z `elem` (init.tail.sort) [w,x,y,z]
median4 :: (a -> a -> Bool) -> a -> a -> a -> a -> Int
median4 lte a b c d =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte b d
            then 1
            else 3
        else
          if inline lte b c
            then 1
            else 2
    else
      if inline lte c d
        then
          if inline lte a d
            then 0
            else 3
        else
          if inline lte a c
            then 0
            else 2
{-# INLINE median4 #-}

-- | Find the median of 5 items, optimally.
--
-- >>> median5 (<=) 1 4 3 2 5
-- 2
-- >>> median5 (<=) 1 3 2 4 5
-- 1
-- >>> median5 (<=) 2 4 1 3 5
-- 3
-- >>> median5 (<=) 3 1 4 2 5
-- 0
-- >>> median5 (<=) 2 1 4 5 3
-- 4
--
-- prop> [v,w,x,y,z] !! median5 (<=) v w x y z  === sort [v,w,x,y,z] !! 2
median5 :: (a -> a -> Bool) -> a -> a -> a -> a -> a -> Int
median5 lte a b c d e =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte a c
            then
              if inline lte e b
                then
                  if inline lte c e
                    then
                      if inline lte e d
                        then 4
                        else 3
                    else
                      if inline lte c b
                        then 2
                        else 1
                else
                  if inline lte c b
                    then
                      if inline lte b d
                        then 1
                        else 3
                    else
                      if inline lte c e
                        then 2
                        else 4
            else
              if inline lte e d
                then
                  if inline lte a e
                    then
                      if inline lte e b
                        then 4
                        else 1
                    else
                      if inline lte a d
                        then 0
                        else 3
                else
                  if inline lte a d
                    then
                      if inline lte d b
                        then 3
                        else 1
                    else
                      if inline lte a e
                        then 0
                        else 4
        else
          if inline lte a d
            then
              if inline lte e b
                then
                  if inline lte d e
                    then
                      if inline lte e c
                        then 4
                        else 2
                    else
                      if inline lte d b
                        then 3
                        else 1
                else
                  if inline lte d b
                    then
                      if inline lte b c
                        then 1
                        else 2
                    else
                      if inline lte d e
                        then 3
                        else 4
            else
              if inline lte e c
                then
                  if inline lte a e
                    then
                      if inline lte e b
                        then 4
                        else 1
                    else
                      if inline lte a c
                        then 0
                        else 2
                else
                  if inline lte a c
                    then
                      if inline lte c b
                        then 2
                        else 1
                    else
                      if inline lte a e
                        then 0
                        else 4
    else
      if inline lte c d
        then
          if inline lte b c
            then
              if inline lte e a
                then
                  if inline lte c e
                    then
                      if inline lte e d
                        then 4
                        else 3
                    else
                      if inline lte c a
                        then 2
                        else 0
                else
                  if inline lte c a
                    then
                      if inline lte a d
                        then 0
                        else 3
                    else
                      if inline lte c e
                        then 2
                        else 4
            else
              if inline lte e d
                then
                  if inline lte b e
                    then
                      if inline lte e a
                        then 4
                        else 0
                    else
                      if inline lte b d
                        then 1
                        else 3
                else
                  if inline lte b d
                    then
                      if inline lte d a
                        then 3
                        else 0
                    else
                      if inline lte b e
                        then 1
                        else 4
        else
          if inline lte b d
            then
              if inline lte e a
                then
                  if inline lte d e
                    then
                      if inline lte e c
                        then 4
                        else 2
                    else
                      if inline lte d a
                        then 3
                        else 0
                else
                  if inline lte d a
                    then
                      if inline lte a c
                        then 0
                        else 2
                    else
                      if inline lte d e
                        then 3
                        else 4
            else
              if inline lte e c
                then
                  if inline lte b e
                    then
                      if inline lte e a
                        then 4
                        else 0
                    else
                      if inline lte b c
                        then 1
                        else 2
                else
                  if inline lte b c
                    then
                      if inline lte c a
                        then 2
                        else 0
                    else
                      if inline lte b e
                        then 1
                        else 4
{-# INLINE median5 #-}
-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.List (sort)
