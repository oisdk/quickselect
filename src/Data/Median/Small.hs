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

module Data.Median.Small
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
median3 lte _0 _1 _2 =
  if inline lte _0 _1
    then
      if inline lte _0 _2
        then
          if inline lte _1 _2
            then 1
            else 2
        else 0
    else
      if inline lte _1 _2
        then
          if inline lte _0 _2
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
median4 lte _0 _1 _2 _3 =
  if inline lte _0 _1
    then
      if inline lte _2 _3
        then
          if inline lte _1 _3
            then 1
            else 3
        else
          if inline lte _1 _2
            then 1
            else 2
    else
      if inline lte _2 _3
        then
          if inline lte _0 _3
            then 0
            else 3
        else
          if inline lte _0 _2
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
median5 lte _0 _1 _2 _3 _4 =
  if inline lte _0 _1
    then
      if inline lte _2 _3
        then
          if inline lte _0 _2
            then
              if inline lte _1 _3
                then
                  if inline lte _1 _2
                    then
                      if inline lte _0 _4
                        then
                          if inline lte _2 _4
                            then 2
                            else
                              if inline lte _1 _4
                                then 4
                                else 1
                        else 1
                    else
                      if inline lte _0 _4
                        then
                          if inline lte _1 _4
                            then 1
                            else
                              if inline lte _2 _4
                                then 4
                                else 2
                        else 2
                else
                  if inline lte _0 _4
                    then
                      if inline lte _3 _4
                        then 3
                        else
                          if inline lte _2 _4
                            then 4
                            else 2
                    else
                      if inline lte _3 _0
                        then 3
                        else 2
            else
              if inline lte _1 _3
                then
                  if inline lte _2 _4
                    then
                      if inline lte _1 _4
                        then 1
                        else
                          if inline lte _0 _4
                            then 4
                            else 0
                    else
                      if inline lte _1 _2
                        then 1
                        else 0
                else
                  if inline lte _3 _0
                    then
                      if inline lte _2 _4
                        then
                          if inline lte _0 _4
                            then 0
                            else
                              if inline lte _3 _4
                                then 4
                                else 3
                        else 3
                    else
                      if inline lte _2 _4
                        then
                          if inline lte _3 _4
                            then 3
                            else
                              if inline lte _0 _4
                                then 4
                                else 0
                        else 0
        else
          if inline lte _0 _3
            then
              if inline lte _1 _2
                then
                  if inline lte _1 _3
                    then
                      if inline lte _0 _4
                        then
                          if inline lte _3 _4
                            then 3
                            else
                              if inline lte _1 _4
                                then 4
                                else 1
                        else 1
                    else
                      if inline lte _0 _4
                        then
                          if inline lte _1 _4
                            then 1
                            else
                              if inline lte _3 _4
                                then 4
                                else 3
                        else 3
                else
                  if inline lte _0 _4
                    then
                      if inline lte _2 _4
                        then 2
                        else
                          if inline lte _3 _4
                            then 4
                            else 3
                    else
                      if inline lte _2 _0
                        then 2
                        else 3
            else
              if inline lte _1 _2
                then
                  if inline lte _3 _4
                    then
                      if inline lte _1 _4
                        then 1
                        else
                          if inline lte _0 _4
                            then 4
                            else 0
                    else
                      if inline lte _1 _3
                        then 1
                        else 0
                else
                  if inline lte _2 _0
                    then
                      if inline lte _3 _4
                        then
                          if inline lte _0 _4
                            then 0
                            else
                              if inline lte _2 _4
                                then 4
                                else 2
                        else 2
                    else
                      if inline lte _3 _4
                        then
                          if inline lte _2 _4
                            then 2
                            else
                              if inline lte _0 _4
                                then 4
                                else 0
                        else 0
    else
      if inline lte _2 _3
        then
          if inline lte _1 _2
            then
              if inline lte _0 _3
                then
                  if inline lte _0 _2
                    then
                      if inline lte _1 _4
                        then
                          if inline lte _2 _4
                            then 2
                            else
                              if inline lte _0 _4
                                then 4
                                else 0
                        else 0
                    else
                      if inline lte _1 _4
                        then
                          if inline lte _0 _4
                            then 0
                            else
                              if inline lte _2 _4
                                then 4
                                else 2
                        else 2
                else
                  if inline lte _1 _4
                    then
                      if inline lte _3 _4
                        then 3
                        else
                          if inline lte _2 _4
                            then 4
                            else 2
                    else
                      if inline lte _3 _1
                        then 3
                        else 2
            else
              if inline lte _0 _3
                then
                  if inline lte _2 _4
                    then
                      if inline lte _0 _4
                        then 0
                        else
                          if inline lte _1 _4
                            then 4
                            else 1
                    else
                      if inline lte _0 _2
                        then 0
                        else 1
                else
                  if inline lte _3 _1
                    then
                      if inline lte _2 _4
                        then
                          if inline lte _1 _4
                            then 1
                            else
                              if inline lte _3 _4
                                then 4
                                else 3
                        else 3
                    else
                      if inline lte _2 _4
                        then
                          if inline lte _3 _4
                            then 3
                            else
                              if inline lte _1 _4
                                then 4
                                else 1
                        else 1
        else
          if inline lte _1 _3
            then
              if inline lte _0 _2
                then
                  if inline lte _0 _3
                    then
                      if inline lte _1 _4
                        then
                          if inline lte _3 _4
                            then 3
                            else
                              if inline lte _0 _4
                                then 4
                                else 0
                        else 0
                    else
                      if inline lte _1 _4
                        then
                          if inline lte _0 _4
                            then 0
                            else
                              if inline lte _3 _4
                                then 4
                                else 3
                        else 3
                else
                  if inline lte _1 _4
                    then
                      if inline lte _2 _4
                        then 2
                        else
                          if inline lte _3 _4
                            then 4
                            else 3
                    else
                      if inline lte _2 _1
                        then 2
                        else 3
            else
              if inline lte _0 _2
                then
                  if inline lte _3 _4
                    then
                      if inline lte _0 _4
                        then 0
                        else
                          if inline lte _1 _4
                            then 4
                            else 1
                    else
                      if inline lte _0 _3
                        then 0
                        else 1
                else
                  if inline lte _2 _1
                    then
                      if inline lte _3 _4
                        then
                          if inline lte _1 _4
                            then 1
                            else
                              if inline lte _2 _4
                                then 4
                                else 2
                        else 2
                    else
                      if inline lte _3 _4
                        then
                          if inline lte _2 _4
                            then 2
                            else
                              if inline lte _1 _4
                                then 4
                                else 1
                        else 1
{-# INLINE median5 #-}
-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.List (sort)
