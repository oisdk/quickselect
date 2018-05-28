module Data.Median.Small where

import GHC.Exts (inline)

-- |
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

-- |
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

-- |
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
