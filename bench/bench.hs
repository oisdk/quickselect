module Main (main) where

import           Criterion.Main

import           Data.Vector        (Vector)
import qualified Data.Vector        as Vector
import qualified Data.Vector.Unboxed as UVector

import           Control.Monad
import           System.Random

import           Control.Arrow ((&&&))

import           Data.Select.Vector
import qualified Data.Select.Vector.Unboxed as Unboxed

int :: Int -> IO Int
int n = randomRIO (0,n)

vec :: Int -> IO (Vector Int, UVector.Vector Int)
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
             [ bench "boxed" $ nf (select (n `div` 2)) xs
             , bench "unboxed" $ nf (Unboxed.select (n `div` 2)) ys]

main :: IO ()
main = defaultMain (map benchAtSize (take 6 (iterate (10*) 100)))
