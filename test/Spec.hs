{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Data.List                  (sort)
import qualified Data.Vector                as Vector
import qualified Data.Vector.Unboxed        as UVector

import           Data.Select.Vector
import qualified Data.Select.Vector.Unboxed as U

prop_median :: Property
prop_median =
    property $
    do xs <-
           forAll
               (Gen.list
                    (Range.linear 1 100)
                    (Gen.int (Range.linear 0 100)))
       let l = length xs
       i <- forAll (Gen.int (Range.linear 0 (l-1)))
       let ys = sort xs
       (ys !! i) === select i (Vector.fromList ys)
       (ys !! i) === U.select i (UVector.fromList ys)

main :: IO Bool
main = checkParallel $$(discover)
