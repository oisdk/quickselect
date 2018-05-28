{-# LANGUAGE CPP #-}
{-# options_ghc -fsimpl-tick-factor=10000 #-}

module Main (main) where

import           Test.QuickCheck

import           Data.List                  (sort)
import qualified Data.Vector                as Vector
import qualified Data.Vector.Unboxed        as UVector

import qualified Data.Select.Quick  as Quick
import qualified Data.Select.Median as Median
import qualified Data.Select.Intro  as Intro

import qualified Data.Select.Unboxed.Quick  as UQuick
import qualified Data.Select.Unboxed.Median as UMedian
import qualified Data.Select.Unboxed.Intro  as UIntro

main :: IO ()
main =
    quickCheck $
    do NonEmpty xs <- arbitrary :: Gen (NonEmptyList Int)
       i <- choose (0, length xs - 1)
       let ys = sort xs
       let y  = ys !! i
       let vs = Vector.fromList xs
       let us = UVector.fromList xs
#if MIN_VERSION_base(4,8,0)
       pure $
#else
       return $
#endif
           conjoin
               [ y === Quick.select   i vs
               , y === Median.select  i vs
               , y === Intro.select   i vs
               , y === UQuick.select  i us
               , y === UMedian.select i us
               , y === UIntro.select  i us
               ]
