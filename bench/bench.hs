module Main (main) where

import           Criterion.Main

import qualified Data.Vector        as Vector

import           Control.Monad
import           System.Random

import qualified Data.Select.Intro  as Intro
import qualified Data.Select.Median as Median
import qualified Data.Select.Quick  as Quick

int :: Int -> IO Int
int n = randomRIO (0,n)

vec :: Int -> IO (Vector.Vector Int)
vec n =
    fmap
        Vector.fromList
        (replicateM n (int n))

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (vec n) $
    \xs ->
         bgroup
             (show n)
             [ bench "intro" $ nf (Intro.select (n `div` 2)) xs
             , bench "median" $ nf (Median.select (n `div` 2)) xs
             , bench "quick" $ nf (Quick.select (n `div` 2)) xs ]

main :: IO ()
main = defaultMain (map benchAtSize (take 6 (iterate (10*) 100)))
