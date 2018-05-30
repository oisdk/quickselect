module Data.Median.Mutable.Optimal where

import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as Vector

import           Control.Monad.ST
import           Control.Monad (when,unless)

import GHC.Exts (inline)

median3 ::  MVector v a => (a -> a -> Bool) -> v s a -> Int -> Int -> Int -> ST s ()
median3 lte vc xs ys zs = do
    x <- Vector.unsafeRead vc xs
    y <- Vector.unsafeRead vc ys
    z <- Vector.unsafeRead vc zs
    if inline lte x y
        then if inline lte x z
                 then if inline lte y z
                          then pure ()
                          else do
                              Vector.unsafeWrite vc ys z
                              Vector.unsafeWrite vc zs y
                 else do
                     Vector.unsafeWrite vc xs z
                     Vector.unsafeWrite vc ys x
                     Vector.unsafeWrite vc zs y
        else if inline lte y z
                 then do
                     Vector.unsafeWrite vc xs y
                     if inline lte x z
                         then Vector.unsafeWrite vc ys x
                         else do
                             Vector.unsafeWrite vc ys z
                             Vector.unsafeWrite vc zs x
                 else do
                     Vector.unsafeWrite vc xs z
                     Vector.unsafeWrite vc zs x

median5 :: MVector v a => (a -> a -> Bool) -> v s a -> Int -> Int -> Int -> Int -> Int -> ST s ()
median5 lte vc vs ws xs ys zs = do
  v <- Vector.unsafeRead vc vs
  w <- Vector.unsafeRead vc ws
  x <- Vector.unsafeRead vc xs
  y <- Vector.unsafeRead vc ys
  z <- Vector.unsafeRead vc zs
  if inline lte v x
    then
      if inline lte w y
        then
          if inline lte x y
            then
              if inline lte w z
                then
                  if inline lte x z
                    then
                      unless (inline lte w x) $ do
                        Vector.unsafeWrite vc ws x
                        Vector.unsafeWrite vc xs w
                    else do
                      Vector.unsafeWrite vc zs x
                      if inline lte v z
                        then Vector.unsafeWrite vc xs z
                        else do
                          Vector.unsafeWrite vc vs z
                          Vector.unsafeWrite vc xs v
                else
                  if inline lte x w
                    then do
                      Vector.unsafeWrite vc zs w
                      if inline lte z x
                        then Vector.unsafeWrite vc ws z
                        else do
                          Vector.unsafeWrite vc ws x
                          Vector.unsafeWrite vc xs z
                    else do
                      Vector.unsafeWrite vc ws z
                      Vector.unsafeWrite vc zs x
                      if inline lte v w
                        then Vector.unsafeWrite vc xs w
                        else do
                          Vector.unsafeWrite vc vs w
                          Vector.unsafeWrite vc xs v
            else do
              Vector.unsafeWrite vc ys x
              if inline lte v z
                then
                  if inline lte y z
                    then do
                      Vector.unsafeWrite vc vs w
                      if inline lte v y
                        then do
                          Vector.unsafeWrite vc ws v
                          Vector.unsafeWrite vc xs y
                        else do
                          Vector.unsafeWrite vc ws y
                          Vector.unsafeWrite vc xs v
                    else do
                      Vector.unsafeWrite vc ws v
                      Vector.unsafeWrite vc zs y
                      if inline lte w z
                        then do
                          Vector.unsafeWrite vc vs w
                          Vector.unsafeWrite vc xs z
                        else do
                          Vector.unsafeWrite vc vs z
                          Vector.unsafeWrite vc xs w
                else
                  if inline lte y v
                    then do
                      Vector.unsafeWrite vc vs w
                      Vector.unsafeWrite vc zs v
                      if inline lte z y
                        then do
                          Vector.unsafeWrite vc ws z
                          Vector.unsafeWrite vc xs y
                        else do
                          Vector.unsafeWrite vc ws y
                          Vector.unsafeWrite vc xs z
                    else do
                      Vector.unsafeWrite vc ws z
                      Vector.unsafeWrite vc zs y
                      if inline lte w v
                        then do
                          Vector.unsafeWrite vc vs w
                          Vector.unsafeWrite vc xs v
                        else Vector.unsafeWrite vc xs w
        else
          if inline lte x w
            then do
              Vector.unsafeWrite vc ys w
              if inline lte y z
                then
                  if inline lte x z
                    then
                      if inline lte y x
                        then Vector.unsafeWrite vc ws y
                        else do
                          Vector.unsafeWrite vc ws x
                          Vector.unsafeWrite vc xs y
                    else do
                      Vector.unsafeWrite vc ws y
                      Vector.unsafeWrite vc zs x
                      if inline lte v z
                        then Vector.unsafeWrite vc xs z
                        else do
                          Vector.unsafeWrite vc vs z
                          Vector.unsafeWrite vc xs v
                else
                  if inline lte x y
                    then do
                      Vector.unsafeWrite vc zs y
                      if inline lte z x
                        then Vector.unsafeWrite vc ws z
                        else do
                          Vector.unsafeWrite vc ws x
                          Vector.unsafeWrite vc xs z
                    else do
                      Vector.unsafeWrite vc ws z
                      Vector.unsafeWrite vc zs x
                      if inline lte v y
                        then Vector.unsafeWrite vc xs y
                        else do
                          Vector.unsafeWrite vc vs y
                          Vector.unsafeWrite vc xs v
            else do
              Vector.unsafeWrite vc ys x
              if inline lte v z
                then
                  if inline lte w z
                    then do
                      Vector.unsafeWrite vc vs y
                      if inline lte v w
                        then do
                          Vector.unsafeWrite vc ws v
                          Vector.unsafeWrite vc xs w
                        else Vector.unsafeWrite vc xs v
                    else do
                      Vector.unsafeWrite vc ws v
                      Vector.unsafeWrite vc zs w
                      if inline lte y z
                        then do
                          Vector.unsafeWrite vc vs y
                          Vector.unsafeWrite vc xs z
                        else do
                          Vector.unsafeWrite vc vs z
                          Vector.unsafeWrite vc xs y
                else
                  if inline lte w v
                    then do
                      Vector.unsafeWrite vc vs y
                      Vector.unsafeWrite vc zs v
                      if inline lte z w
                        then do
                          Vector.unsafeWrite vc ws z
                          Vector.unsafeWrite vc xs w
                        else Vector.unsafeWrite vc xs z
                    else do
                      Vector.unsafeWrite vc ws z
                      Vector.unsafeWrite vc zs w
                      if inline lte y v
                        then do
                          Vector.unsafeWrite vc vs y
                          Vector.unsafeWrite vc xs v
                        else Vector.unsafeWrite vc xs y
    else
      if inline lte w y
        then
          if inline lte v y
            then
              if inline lte w z
                then
                  if inline lte v z
                    then do
                      Vector.unsafeWrite vc vs x
                      if inline lte w v
                        then Vector.unsafeWrite vc xs v
                        else do
                          Vector.unsafeWrite vc ws v
                          Vector.unsafeWrite vc xs w
                    else do
                      Vector.unsafeWrite vc zs v
                      if inline lte x z
                        then do
                          Vector.unsafeWrite vc vs x
                          Vector.unsafeWrite vc xs z
                        else Vector.unsafeWrite vc vs z
                else
                  if inline lte v w
                    then do
                      Vector.unsafeWrite vc vs x
                      Vector.unsafeWrite vc zs w
                      if inline lte z v
                        then do
                          Vector.unsafeWrite vc ws z
                          Vector.unsafeWrite vc xs v
                        else do
                          Vector.unsafeWrite vc ws v
                          Vector.unsafeWrite vc xs z
                    else do
                      Vector.unsafeWrite vc ws z
                      Vector.unsafeWrite vc zs v
                      if inline lte x w
                        then do
                          Vector.unsafeWrite vc vs x
                          Vector.unsafeWrite vc xs w
                        else Vector.unsafeWrite vc vs w
            else do
              Vector.unsafeWrite vc ys v
              if inline lte x z
                then
                  if inline lte y z
                    then do
                      Vector.unsafeWrite vc vs w
                      if inline lte x y
                        then do
                          Vector.unsafeWrite vc ws x
                          Vector.unsafeWrite vc xs y
                        else Vector.unsafeWrite vc ws y
                    else do
                      Vector.unsafeWrite vc ws x
                      Vector.unsafeWrite vc zs y
                      if inline lte w z
                        then do
                          Vector.unsafeWrite vc vs w
                          Vector.unsafeWrite vc xs z
                        else do
                          Vector.unsafeWrite vc vs z
                          Vector.unsafeWrite vc xs w
                else
                  if inline lte y x
                    then do
                      Vector.unsafeWrite vc vs w
                      Vector.unsafeWrite vc zs x
                      if inline lte z y
                        then do
                          Vector.unsafeWrite vc ws z
                          Vector.unsafeWrite vc xs y
                        else do
                          Vector.unsafeWrite vc ws y
                          Vector.unsafeWrite vc xs z
                    else do
                      Vector.unsafeWrite vc ws z
                      Vector.unsafeWrite vc zs y
                      if inline lte w x
                        then Vector.unsafeWrite vc vs w
                        else do
                          Vector.unsafeWrite vc vs x
                          Vector.unsafeWrite vc xs w
        else
          if inline lte v w
            then do
              Vector.unsafeWrite vc ys w
              if inline lte y z
                then
                  if inline lte v z
                    then do
                      Vector.unsafeWrite vc vs x
                      if inline lte y v
                        then do
                          Vector.unsafeWrite vc ws y
                          Vector.unsafeWrite vc xs v
                        else do
                          Vector.unsafeWrite vc ws v
                          Vector.unsafeWrite vc xs y
                    else do
                      Vector.unsafeWrite vc ws y
                      Vector.unsafeWrite vc zs v
                      if inline lte x z
                        then do
                          Vector.unsafeWrite vc vs x
                          Vector.unsafeWrite vc xs z
                        else Vector.unsafeWrite vc vs z
                else
                  if inline lte v y
                    then do
                      Vector.unsafeWrite vc vs x
                      Vector.unsafeWrite vc zs y
                      if inline lte z v
                        then do
                          Vector.unsafeWrite vc ws z
                          Vector.unsafeWrite vc xs v
                        else do
                          Vector.unsafeWrite vc ws v
                          Vector.unsafeWrite vc xs z
                    else do
                      Vector.unsafeWrite vc ws z
                      Vector.unsafeWrite vc zs v
                      if inline lte x y
                        then do
                          Vector.unsafeWrite vc vs x
                          Vector.unsafeWrite vc xs y
                        else Vector.unsafeWrite vc vs y
            else do
              Vector.unsafeWrite vc ys v
              if inline lte x z
                then
                  if inline lte w z
                    then do
                      Vector.unsafeWrite vc vs y
                      when (inline lte x w) $ do
                        Vector.unsafeWrite vc ws x
                        Vector.unsafeWrite vc xs w
                    else do
                      Vector.unsafeWrite vc ws x
                      Vector.unsafeWrite vc zs w
                      if inline lte y z
                        then do
                          Vector.unsafeWrite vc vs y
                          Vector.unsafeWrite vc xs z
                        else do
                          Vector.unsafeWrite vc vs z
                          Vector.unsafeWrite vc xs y
                else
                  if inline lte w x
                    then do
                      Vector.unsafeWrite vc vs y
                      Vector.unsafeWrite vc zs x
                      if inline lte z w
                        then do
                          Vector.unsafeWrite vc ws z
                          Vector.unsafeWrite vc xs w
                        else Vector.unsafeWrite vc xs z
                    else do
                      Vector.unsafeWrite vc ws z
                      Vector.unsafeWrite vc zs w
                      if inline lte y x
                        then Vector.unsafeWrite vc vs y
                        else do
                          Vector.unsafeWrite vc vs x
                          Vector.unsafeWrite vc xs y
