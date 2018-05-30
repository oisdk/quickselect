{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Data.List                  (sort)
import qualified Data.Vector                as Vector
import qualified Data.Vector.Unboxed        as UVector

import qualified Data.Select.Intro          as Intro
import qualified Data.Select.Median         as Median
import qualified Data.Select.Mutable.Quick  as MQuick
import qualified Data.Select.Quick          as Quick

import qualified Data.Select.Unboxed.Intro  as UIntro
import qualified Data.Select.Unboxed.Median as UMedian
import qualified Data.Select.Unboxed.Quick  as UQuick

import           Control.Monad.ST
import           Data.Functor
import           Data.Foldable

mutableSelectProp
    :: (forall s. (Int -> Int -> Bool) -> Vector.MVector s Int -> Int -> Int -> Int -> ST s Int)
    -> Property
mutableSelectProp fn =
    property $
    do xs <- forAll (Gen.list (Range.linear  1 100) (Gen.int (Range.linear 0 10)))
       let sz = length xs
       lb <- forAll (Gen.int (Range.linear 0 (sz-1)))
       ub <- forAll (Gen.int (Range.linear lb (sz-1)))
       n <- forAll (Gen.int (Range.linear lb ub))
       let vs = Vector.fromList xs
       let ps = Vector.modify (\ys -> void (fn (<=) ys lb ub n)) vs
       annotateShow ps
       Vector.take lb vs === Vector.take lb ps
       Vector.drop (ub+1) vs === Vector.drop (ub+1) ps
       let below = Vector.slice lb (n-lb) ps
       annotateShow below
       for_ below $ \x -> assert (x <= ps Vector.! n)
       -- property $ all (<= (ps Vector.! n)) (Vector.slice lb n ps)
       -- property $ all (> (ps Vector.! n)) (Vector.slice (lb+n) (ub - (lb+n)) ps)

prop_quick :: Property
prop_quick = mutableSelectProp MQuick.select


prop_select :: Property
prop_select = property $
    do xs <- forAll (Gen.list (Range.linear 1 100) (Gen.int (Range.linear 0 10)))
       i <- forAll (Gen.int (Range.linear 0 (length xs - 1)))
       let ys = sort xs
       let y  = ys !! i
       let vs = Vector.fromList xs
       let us = UVector.fromList xs
       y === Quick.select   i vs
       y === Median.select  i vs
       y === Intro.select   i vs
       y === UQuick.select  i us
       y === UMedian.select i us
       y === UIntro.select  i us

main :: IO Bool
main = checkParallel $$(discover)
