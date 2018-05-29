module Main (main) where

import           Criterion.Main

import qualified Data.Vector         as Vector
import qualified Data.Vector.Unboxed as UVector

import           Control.Arrow ((&&&))
import           Control.Monad
import           System.Random

import qualified Data.Select.Intro  as Intro
import qualified Data.Select.Median as Median
import qualified Data.Select.Quick  as Quick
import qualified Data.Select.Unboxed.Intro  as UIntro
import qualified Data.Select.Unboxed.Median as UMedian
import qualified Data.Select.Unboxed.Quick  as UQuick

int :: Int -> IO Int
int n = randomRIO (0,n)

vec :: Int -> IO (Vector.Vector Int, UVector.Vector Int)
vec n =
    fmap
        (Vector.fromList &&& UVector.fromList)
        (replicateM n (int n))

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (vec n) $
    \ ~(xs,ys) ->
         bgroup
             (show n)
             [ bench "intro"   $ nf (Intro.select   (n `div` 2)) xs
             , bench "median"  $ nf (Median.select  (n `div` 2)) xs
             , bench "quick"   $ nf (Quick.select   (n `div` 2)) xs
             , bench "uintro"  $ nf (UIntro.select  (n `div` 2)) ys
             , bench "umedian" $ nf (UMedian.select (n `div` 2)) ys
             , bench "uquick"  $ nf (UQuick.select  (n `div` 2)) ys
             ]

main :: IO ()
main = defaultMain (map benchAtSize (take 3 (iterate (10*) 100)))
