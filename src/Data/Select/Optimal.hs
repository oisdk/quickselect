{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Select.Optimal
-- Description : Optimal selection functions for small input.
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides optimal selection functions for small,
-- fixed-size inputs. Each function returns the (0-based) index of the
-- argument which is the nth item, according to the supplied relation.
module Data.Select.Optimal
  (select2
  ,select3)
  where

import GHC.Exts (inline)

{-# INLINE select2 #-}
{-# INLINE select3 #-}

-- | Select from 2 items.
select2 :: (a -> a -> Bool) -> Int -> a -> a -> Int
select2 lte 0 a b = if inline lte a b then 0 else 1
select2 lte 1 a b = if inline lte a b then 1 else 0
select2 _ _ _ _ =
#if MIN_VERSION_base(4,9,0)
    errorWithoutStackTrace
#else
    error
#endif
    "Data.Select.Optimal.select2: index out of bounds."

-- | Select from 3 items.
select3 :: (a -> a -> Bool) -> Int -> a -> a -> a -> Int
select3 lte 0 a b c =
  if inline lte a b
    then
      if inline lte a c
        then 0
        else 2
    else
      if inline lte b c
        then 1
        else 2
select3 lte 1 a b c =
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
select3 lte 2 a b c =
  if inline lte a b
    then
      if inline lte a c
        then
          if inline lte b c
            then 2
            else 1
        else 1
    else
      if inline lte b c
        then
          if inline lte a c
            then 2
            else 0
        else 0
select3 _ _ _ _ _ =
#if MIN_VERSION_base(4,9,0)
    errorWithoutStackTrace
#else
    error
#endif
    "Data.Select.Optimal.select3: index out of bounds."
