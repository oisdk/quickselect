{-# LANGUAGE CPP #-}

module Main (main) where

import           Test.QuickCheck

import           Data.List                  (sort)
import qualified Data.Vector                as Vector

import qualified Data.Select.Quick as Quick
import qualified Data.Select.Median as Median
import qualified Data.Select.Intro as Intro

main :: IO ()
main =
    quickCheck $
    do NonEmpty xs <- arbitrary :: Gen (NonEmptyList Int)
       i <- choose (0, length xs - 1)
       let ys = sort xs
       let y  = ys !! i
       let vs = Vector.fromList xs
#if MIN_VERSION_base(4,8,0)
       pure $
#else
       return $
#endif
           conjoin
               [ y === Quick.select i vs
               , y === Median.select i vs
               , y === Intro.select i vs
               ]
