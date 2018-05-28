{-# LANGUAGE CPP #-}

module Data.Select.Small
  (select2
  ,select3
  ,select4
  ,select5)
  where

import GHC.Exts (inline)

{-# INLINE select3 #-}
{-# INLINE select4 #-}
{-# INLINE select5 #-}

select2 :: (a -> a -> Bool) -> Int -> a -> a -> Int
select2 lte 0 a b = if inline lte a b then 0 else 1
select2 lte 1 a b = if inline lte a b then 1 else 0
select2 _ _ _ _ = errorWithoutStackTrace "Data.Select.Small.select2: index out of bounds."

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
    "Data.Select.Small.select3: index out of bounds."

select4 :: (a -> a -> Bool) -> Int -> a -> a -> a -> a -> Int
select4 lte 0 a b c d =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte a c
            then 0
            else 2
        else
          if inline lte a d
            then 0
            else 3
    else
      if inline lte c d
        then
          if inline lte b c
            then 1
            else 2
        else
          if inline lte b d
            then 1
            else 3
select4 lte 1 a b c d =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte a c
            then
              if inline lte b d
                then
                  if inline lte b c
                    then 1
                    else 2
                else 2
            else
              if inline lte b d
                then 0
                else
                  if inline lte d a
                    then 3
                    else 0
        else
          if inline lte a d
            then
              if inline lte b c
                then
                  if inline lte b d
                    then 1
                    else 3
                else 3
            else
              if inline lte b c
                then 0
                else
                  if inline lte c a
                    then 2
                    else 0
    else
      if inline lte c d
        then
          if inline lte b c
            then
              if inline lte a d
                then
                  if inline lte a c
                    then 0
                    else 2
                else 2
            else
              if inline lte a d
                then 1
                else
                  if inline lte d b
                    then 3
                    else 1
        else
          if inline lte b d
            then
              if inline lte a c
                then
                  if inline lte a d
                    then 0
                    else 3
                else 3
            else
              if inline lte a c
                then 1
                else
                  if inline lte c b
                    then 2
                    else 1
select4 lte 2 a b c d =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte a c
            then
              if inline lte b d
                then
                  if inline lte b c
                    then 2
                    else 1
                else 3
            else
              if inline lte b d
                then 1
                else
                  if inline lte d a
                    then 0
                    else 3
        else
          if inline lte a d
            then
              if inline lte b c
                then
                  if inline lte b d
                    then 3
                    else 1
                else 2
            else
              if inline lte b c
                then 1
                else
                  if inline lte c a
                    then 0
                    else 2
    else
      if inline lte c d
        then
          if inline lte b c
            then
              if inline lte a d
                then
                  if inline lte a c
                    then 2
                    else 0
                else 3
            else
              if inline lte a d
                then 0
                else
                  if inline lte d b
                    then 1
                    else 3
        else
          if inline lte b d
            then
              if inline lte a c
                then
                  if inline lte a d
                    then 3
                    else 0
                else 2
            else
              if inline lte a c
                then 0
                else
                  if inline lte c b
                    then 1
                    else 2
select4 lte 3 a b c d =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte b d
            then 3
            else 1
        else
          if inline lte b c
            then 2
            else 1
    else
      if inline lte c d
        then
          if inline lte a d
            then 3
            else 0
        else
          if inline lte a c
            then 2
            else 0
select4 _ _ _ _ _ _ =
#if MIN_VERSION_base(4,9,0)
    errorWithoutStackTrace
#else
    error
#endif
    "Data.Select.Small.select4: index out of bounds"

select5 :: (a -> a -> Bool) -> Int -> a -> a -> a -> a -> a -> Int
select5 lte 0 a b c d e =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte a c
            then
              if inline lte a e
                then 0
                else 4
            else
              if inline lte c e
                then 2
                else 4
        else
          if inline lte a d
            then
              if inline lte a e
                then 0
                else 4
            else
              if inline lte d e
                then 3
                else 4
    else
      if inline lte c d
        then
          if inline lte b c
            then
              if inline lte b e
                then 1
                else 4
            else
              if inline lte c e
                then 2
                else 4
        else
          if inline lte b d
            then
              if inline lte b e
                then 1
                else 4
            else
              if inline lte d e
                then 3
                else 4
select5 lte 1 a b c d e =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte a c
            then
              if inline lte b d
                then
                  if inline lte b c
                    then
                      if inline lte a e
                        then
                          if inline lte c e
                            then 1
                            else
                              if inline lte b e
                                then 1
                                else 4
                        else 0
                    else
                      if inline lte a e
                        then
                          if inline lte b e
                            then 2
                            else
                              if inline lte c e
                                then 2
                                else 4
                        else 0
                else
                  if inline lte a e
                    then
                      if inline lte d e
                        then 2
                        else
                          if inline lte c e
                            then 2
                            else 4
                    else
                      if inline lte d a
                        then 2
                        else 0
            else
              if inline lte b d
                then
                  if inline lte c e
                    then
                      if inline lte b e
                        then 0
                        else
                          if inline lte a e
                            then 0
                            else 4
                    else
                      if inline lte b c
                        then 0
                        else 2
                else
                  if inline lte d a
                    then
                      if inline lte c e
                        then
                          if inline lte a e
                            then 3
                            else
                              if inline lte d e
                                then 3
                                else 4
                        else 2
                    else
                      if inline lte c e
                        then
                          if inline lte d e
                            then 0
                            else
                              if inline lte a e
                                then 0
                                else 4
                        else 2
        else
          if inline lte a d
            then
              if inline lte b c
                then
                  if inline lte b d
                    then
                      if inline lte a e
                        then
                          if inline lte d e
                            then 1
                            else
                              if inline lte b e
                                then 1
                                else 4
                        else 0
                    else
                      if inline lte a e
                        then
                          if inline lte b e
                            then 3
                            else
                              if inline lte d e
                                then 3
                                else 4
                        else 0
                else
                  if inline lte a e
                    then
                      if inline lte c e
                        then 3
                        else
                          if inline lte d e
                            then 3
                            else 4
                    else
                      if inline lte c a
                        then 3
                        else 0
            else
              if inline lte b c
                then
                  if inline lte d e
                    then
                      if inline lte b e
                        then 0
                        else
                          if inline lte a e
                            then 0
                            else 4
                    else
                      if inline lte b d
                        then 0
                        else 3
                else
                  if inline lte c a
                    then
                      if inline lte d e
                        then
                          if inline lte a e
                            then 2
                            else
                              if inline lte c e
                                then 2
                                else 4
                        else 3
                    else
                      if inline lte d e
                        then
                          if inline lte c e
                            then 0
                            else
                              if inline lte a e
                                then 0
                                else 4
                        else 3
    else
      if inline lte c d
        then
          if inline lte b c
            then
              if inline lte a d
                then
                  if inline lte a c
                    then
                      if inline lte b e
                        then
                          if inline lte c e
                            then 0
                            else
                              if inline lte a e
                                then 0
                                else 4
                        else 1
                    else
                      if inline lte b e
                        then
                          if inline lte a e
                            then 2
                            else
                              if inline lte c e
                                then 2
                                else 4
                        else 1
                else
                  if inline lte b e
                    then
                      if inline lte d e
                        then 2
                        else
                          if inline lte c e
                            then 2
                            else 4
                    else
                      if inline lte d b
                        then 2
                        else 1
            else
              if inline lte a d
                then
                  if inline lte c e
                    then
                      if inline lte a e
                        then 1
                        else
                          if inline lte b e
                            then 1
                            else 4
                    else
                      if inline lte a c
                        then 1
                        else 2
                else
                  if inline lte d b
                    then
                      if inline lte c e
                        then
                          if inline lte b e
                            then 3
                            else
                              if inline lte d e
                                then 3
                                else 4
                        else 2
                    else
                      if inline lte c e
                        then
                          if inline lte d e
                            then 1
                            else
                              if inline lte b e
                                then 1
                                else 4
                        else 2
        else
          if inline lte b d
            then
              if inline lte a c
                then
                  if inline lte a d
                    then
                      if inline lte b e
                        then
                          if inline lte d e
                            then 0
                            else
                              if inline lte a e
                                then 0
                                else 4
                        else 1
                    else
                      if inline lte b e
                        then
                          if inline lte a e
                            then 3
                            else
                              if inline lte d e
                                then 3
                                else 4
                        else 1
                else
                  if inline lte b e
                    then
                      if inline lte c e
                        then 3
                        else
                          if inline lte d e
                            then 3
                            else 4
                    else
                      if inline lte c b
                        then 3
                        else 1
            else
              if inline lte a c
                then
                  if inline lte d e
                    then
                      if inline lte a e
                        then 1
                        else
                          if inline lte b e
                            then 1
                            else 4
                    else
                      if inline lte a d
                        then 1
                        else 3
                else
                  if inline lte c b
                    then
                      if inline lte d e
                        then
                          if inline lte b e
                            then 2
                            else
                              if inline lte c e
                                then 2
                                else 4
                        else 3
                    else
                      if inline lte d e
                        then
                          if inline lte c e
                            then 1
                            else
                              if inline lte b e
                                then 1
                                else 4
                        else 3
select5 lte 2 a b c d e =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte a c
            then
              if inline lte b d
                then
                  if inline lte b c
                    then
                      if inline lte a e
                        then
                          if inline lte c e
                            then 2
                            else
                              if inline lte b e
                                then 4
                                else 1
                        else 1
                    else
                      if inline lte a e
                        then
                          if inline lte b e
                            then 1
                            else
                              if inline lte c e
                                then 4
                                else 2
                        else 2
                else
                  if inline lte a e
                    then
                      if inline lte d e
                        then 3
                        else
                          if inline lte c e
                            then 4
                            else 2
                    else
                      if inline lte d a
                        then 3
                        else 2
            else
              if inline lte b d
                then
                  if inline lte c e
                    then
                      if inline lte b e
                        then 1
                        else
                          if inline lte a e
                            then 4
                            else 0
                    else
                      if inline lte b c
                        then 1
                        else 0
                else
                  if inline lte d a
                    then
                      if inline lte c e
                        then
                          if inline lte a e
                            then 0
                            else
                              if inline lte d e
                                then 4
                                else 3
                        else 3
                    else
                      if inline lte c e
                        then
                          if inline lte d e
                            then 3
                            else
                              if inline lte a e
                                then 4
                                else 0
                        else 0
        else
          if inline lte a d
            then
              if inline lte b c
                then
                  if inline lte b d
                    then
                      if inline lte a e
                        then
                          if inline lte d e
                            then 3
                            else
                              if inline lte b e
                                then 4
                                else 1
                        else 1
                    else
                      if inline lte a e
                        then
                          if inline lte b e
                            then 1
                            else
                              if inline lte d e
                                then 4
                                else 3
                        else 3
                else
                  if inline lte a e
                    then
                      if inline lte c e
                        then 2
                        else
                          if inline lte d e
                            then 4
                            else 3
                    else
                      if inline lte c a
                        then 2
                        else 3
            else
              if inline lte b c
                then
                  if inline lte d e
                    then
                      if inline lte b e
                        then 1
                        else
                          if inline lte a e
                            then 4
                            else 0
                    else
                      if inline lte b d
                        then 1
                        else 0
                else
                  if inline lte c a
                    then
                      if inline lte d e
                        then
                          if inline lte a e
                            then 0
                            else
                              if inline lte c e
                                then 4
                                else 2
                        else 2
                    else
                      if inline lte d e
                        then
                          if inline lte c e
                            then 2
                            else
                              if inline lte a e
                                then 4
                                else 0
                        else 0
    else
      if inline lte c d
        then
          if inline lte b c
            then
              if inline lte a d
                then
                  if inline lte a c
                    then
                      if inline lte b e
                        then
                          if inline lte c e
                            then 2
                            else
                              if inline lte a e
                                then 4
                                else 0
                        else 0
                    else
                      if inline lte b e
                        then
                          if inline lte a e
                            then 0
                            else
                              if inline lte c e
                                then 4
                                else 2
                        else 2
                else
                  if inline lte b e
                    then
                      if inline lte d e
                        then 3
                        else
                          if inline lte c e
                            then 4
                            else 2
                    else
                      if inline lte d b
                        then 3
                        else 2
            else
              if inline lte a d
                then
                  if inline lte c e
                    then
                      if inline lte a e
                        then 0
                        else
                          if inline lte b e
                            then 4
                            else 1
                    else
                      if inline lte a c
                        then 0
                        else 1
                else
                  if inline lte d b
                    then
                      if inline lte c e
                        then
                          if inline lte b e
                            then 1
                            else
                              if inline lte d e
                                then 4
                                else 3
                        else 3
                    else
                      if inline lte c e
                        then
                          if inline lte d e
                            then 3
                            else
                              if inline lte b e
                                then 4
                                else 1
                        else 1
        else
          if inline lte b d
            then
              if inline lte a c
                then
                  if inline lte a d
                    then
                      if inline lte b e
                        then
                          if inline lte d e
                            then 3
                            else
                              if inline lte a e
                                then 4
                                else 0
                        else 0
                    else
                      if inline lte b e
                        then
                          if inline lte a e
                            then 0
                            else
                              if inline lte d e
                                then 4
                                else 3
                        else 3
                else
                  if inline lte b e
                    then
                      if inline lte c e
                        then 2
                        else
                          if inline lte d e
                            then 4
                            else 3
                    else
                      if inline lte c b
                        then 2
                        else 3
            else
              if inline lte a c
                then
                  if inline lte d e
                    then
                      if inline lte a e
                        then 0
                        else
                          if inline lte b e
                            then 4
                            else 1
                    else
                      if inline lte a d
                        then 0
                        else 1
                else
                  if inline lte c b
                    then
                      if inline lte d e
                        then
                          if inline lte b e
                            then 1
                            else
                              if inline lte c e
                                then 4
                                else 2
                        else 2
                    else
                      if inline lte d e
                        then
                          if inline lte c e
                            then 2
                            else
                              if inline lte b e
                                then 4
                                else 1
                        else 1
select5 lte 3 a b c d e =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte a c
            then
              if inline lte b d
                then
                  if inline lte b c
                    then
                      if inline lte a e
                        then
                          if inline lte c e
                            then
                              if inline lte d e
                                then 3
                                else 4
                            else 2
                        else 2
                    else
                      if inline lte a e
                        then
                          if inline lte b e
                            then
                              if inline lte d e
                                then 3
                                else 4
                            else 1
                        else 1
                else
                  if inline lte a e
                    then
                      if inline lte d e
                        then
                          if inline lte b e
                            then 1
                            else 4
                        else 3
                    else
                      if inline lte d a
                        then 0
                        else 3
            else
              if inline lte b d
                then
                  if inline lte c e
                    then
                      if inline lte b e
                        then
                          if inline lte d e
                            then 3
                            else 4
                        else 1
                    else
                      if inline lte b c
                        then 2
                        else 1
                else
                  if inline lte d a
                    then
                      if inline lte c e
                        then
                          if inline lte a e
                            then
                              if inline lte b e
                                then 1
                                else 4
                            else 0
                        else 0
                    else
                      if inline lte c e
                        then
                          if inline lte d e
                            then
                              if inline lte b e
                                then 1
                                else 4
                            else 3
                        else 3
        else
          if inline lte a d
            then
              if inline lte b c
                then
                  if inline lte b d
                    then
                      if inline lte a e
                        then
                          if inline lte d e
                            then
                              if inline lte c e
                                then 2
                                else 4
                            else 3
                        else 3
                    else
                      if inline lte a e
                        then
                          if inline lte b e
                            then
                              if inline lte c e
                                then 2
                                else 4
                            else 1
                        else 1
                else
                  if inline lte a e
                    then
                      if inline lte c e
                        then
                          if inline lte b e
                            then 1
                            else 4
                        else 2
                    else
                      if inline lte c a
                        then 0
                        else 2
            else
              if inline lte b c
                then
                  if inline lte d e
                    then
                      if inline lte b e
                        then
                          if inline lte c e
                            then 2
                            else 4
                        else 1
                    else
                      if inline lte b d
                        then 3
                        else 1
                else
                  if inline lte c a
                    then
                      if inline lte d e
                        then
                          if inline lte a e
                            then
                              if inline lte b e
                                then 1
                                else 4
                            else 0
                        else 0
                    else
                      if inline lte d e
                        then
                          if inline lte c e
                            then
                              if inline lte b e
                                then 1
                                else 4
                            else 2
                        else 2
    else
      if inline lte c d
        then
          if inline lte b c
            then
              if inline lte a d
                then
                  if inline lte a c
                    then
                      if inline lte b e
                        then
                          if inline lte c e
                            then
                              if inline lte d e
                                then 3
                                else 4
                            else 2
                        else 2
                    else
                      if inline lte b e
                        then
                          if inline lte a e
                            then
                              if inline lte d e
                                then 3
                                else 4
                            else 0
                        else 0
                else
                  if inline lte b e
                    then
                      if inline lte d e
                        then
                          if inline lte a e
                            then 0
                            else 4
                        else 3
                    else
                      if inline lte d b
                        then 1
                        else 3
            else
              if inline lte a d
                then
                  if inline lte c e
                    then
                      if inline lte a e
                        then
                          if inline lte d e
                            then 3
                            else 4
                        else 0
                    else
                      if inline lte a c
                        then 2
                        else 0
                else
                  if inline lte d b
                    then
                      if inline lte c e
                        then
                          if inline lte b e
                            then
                              if inline lte a e
                                then 0
                                else 4
                            else 1
                        else 1
                    else
                      if inline lte c e
                        then
                          if inline lte d e
                            then
                              if inline lte a e
                                then 0
                                else 4
                            else 3
                        else 3
        else
          if inline lte b d
            then
              if inline lte a c
                then
                  if inline lte a d
                    then
                      if inline lte b e
                        then
                          if inline lte d e
                            then
                              if inline lte c e
                                then 2
                                else 4
                            else 3
                        else 3
                    else
                      if inline lte b e
                        then
                          if inline lte a e
                            then
                              if inline lte c e
                                then 2
                                else 4
                            else 0
                        else 0
                else
                  if inline lte b e
                    then
                      if inline lte c e
                        then
                          if inline lte a e
                            then 0
                            else 4
                        else 2
                    else
                      if inline lte c b
                        then 1
                        else 2
            else
              if inline lte a c
                then
                  if inline lte d e
                    then
                      if inline lte a e
                        then
                          if inline lte c e
                            then 2
                            else 4
                        else 0
                    else
                      if inline lte a d
                        then 3
                        else 0
                else
                  if inline lte c b
                    then
                      if inline lte d e
                        then
                          if inline lte b e
                            then
                              if inline lte a e
                                then 0
                                else 4
                            else 1
                        else 1
                    else
                      if inline lte d e
                        then
                          if inline lte c e
                            then
                              if inline lte a e
                                then 0
                                else 4
                            else 2
                        else 2
select5 lte 4 a b c d e =
  if inline lte a b
    then
      if inline lte c d
        then
          if inline lte a c
            then
              if inline lte b d
                then
                  if inline lte b c
                    then
                      if inline lte a e
                        then
                          if inline lte c e
                            then
                              if inline lte d e
                                then 4
                                else 3
                            else 3
                        else 3
                    else
                      if inline lte a e
                        then
                          if inline lte b e
                            then
                              if inline lte d e
                                then 4
                                else 3
                            else 3
                        else 3
                else
                  if inline lte a e
                    then
                      if inline lte d e
                        then
                          if inline lte b e
                            then 4
                            else 1
                        else 1
                    else 1
            else
              if inline lte b d
                then
                  if inline lte c e
                    then
                      if inline lte b e
                        then
                          if inline lte d e
                            then 4
                            else 3
                        else 3
                    else 3
                else
                  if inline lte d a
                    then
                      if inline lte c e
                        then
                          if inline lte a e
                            then
                              if inline lte b e
                                then 4
                                else 1
                            else 1
                        else 1
                    else
                      if inline lte c e
                        then
                          if inline lte d e
                            then
                              if inline lte b e
                                then 4
                                else 1
                            else 1
                        else 1
        else
          if inline lte a d
            then
              if inline lte b c
                then
                  if inline lte b d
                    then
                      if inline lte a e
                        then
                          if inline lte d e
                            then
                              if inline lte c e
                                then 4
                                else 2
                            else 2
                        else 2
                    else
                      if inline lte a e
                        then
                          if inline lte b e
                            then
                              if inline lte c e
                                then 4
                                else 2
                            else 2
                        else 2
                else
                  if inline lte a e
                    then
                      if inline lte c e
                        then
                          if inline lte b e
                            then 4
                            else 1
                        else 1
                    else 1
            else
              if inline lte b c
                then
                  if inline lte d e
                    then
                      if inline lte b e
                        then
                          if inline lte c e
                            then 4
                            else 2
                        else 2
                    else 2
                else
                  if inline lte c a
                    then
                      if inline lte d e
                        then
                          if inline lte a e
                            then
                              if inline lte b e
                                then 4
                                else 1
                            else 1
                        else 1
                    else
                      if inline lte d e
                        then
                          if inline lte c e
                            then
                              if inline lte b e
                                then 4
                                else 1
                            else 1
                        else 1
    else
      if inline lte c d
        then
          if inline lte b c
            then
              if inline lte a d
                then
                  if inline lte a c
                    then
                      if inline lte b e
                        then
                          if inline lte c e
                            then
                              if inline lte d e
                                then 4
                                else 3
                            else 3
                        else 3
                    else
                      if inline lte b e
                        then
                          if inline lte a e
                            then
                              if inline lte d e
                                then 4
                                else 3
                            else 3
                        else 3
                else
                  if inline lte b e
                    then
                      if inline lte d e
                        then
                          if inline lte a e
                            then 4
                            else 0
                        else 0
                    else 0
            else
              if inline lte a d
                then
                  if inline lte c e
                    then
                      if inline lte a e
                        then
                          if inline lte d e
                            then 4
                            else 3
                        else 3
                    else 3
                else
                  if inline lte d b
                    then
                      if inline lte c e
                        then
                          if inline lte b e
                            then
                              if inline lte a e
                                then 4
                                else 0
                            else 0
                        else 0
                    else
                      if inline lte c e
                        then
                          if inline lte d e
                            then
                              if inline lte a e
                                then 4
                                else 0
                            else 0
                        else 0
        else
          if inline lte b d
            then
              if inline lte a c
                then
                  if inline lte a d
                    then
                      if inline lte b e
                        then
                          if inline lte d e
                            then
                              if inline lte c e
                                then 4
                                else 2
                            else 2
                        else 2
                    else
                      if inline lte b e
                        then
                          if inline lte a e
                            then
                              if inline lte c e
                                then 4
                                else 2
                            else 2
                        else 2
                else
                  if inline lte b e
                    then
                      if inline lte c e
                        then
                          if inline lte a e
                            then 4
                            else 0
                        else 0
                    else 0
            else
              if inline lte a c
                then
                  if inline lte d e
                    then
                      if inline lte a e
                        then
                          if inline lte c e
                            then 4
                            else 2
                        else 2
                    else 2
                else
                  if inline lte c b
                    then
                      if inline lte d e
                        then
                          if inline lte b e
                            then
                              if inline lte a e
                                then 4
                                else 0
                            else 0
                        else 0
                    else
                      if inline lte d e
                        then
                          if inline lte c e
                            then
                              if inline lte a e
                                then 4
                                else 0
                            else 0
                        else 0
select5 _ _ _ _ _ _ _ =
#if MIN_VERSION_base(4,9,0)
    errorWithoutStackTrace
#else
    error
#endif
    "Data.Select.Small.select5: index out of bounds"
