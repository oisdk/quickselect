module Main (main) where

import           Criterion.Main

import qualified Data.Vector                as Vector
-- import qualified Data.Vector.Unboxed        as UVector

-- import           Control.Arrow              ((&&&))
-- import           Control.DeepSeq
import           Control.Monad
import           Data.List                  (sort, sortBy)
import           System.Random

import qualified Data.Select.Intro          as Intro
import qualified Data.Select.Median         as Median
import qualified Data.Select.Quick          as Quick
-- import qualified Data.Select.Unboxed.Intro  as UIntro
-- import qualified Data.Select.Unboxed.Median as UMedian
-- import qualified Data.Select.Unboxed.Quick  as UQuick

int :: Int -> IO Int
int n = randomRIO (0,n)

vec :: Int -> IO [Vector.Vector Int]
vec n =
    fmap
        (fmap Vector.fromList . sequenceA [id, sort , sortBy (flip compare)])
        (replicateM n (int n))

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (vec n) $
    \xs ->
         bgroup
             (show n)
             [ bgroup
                  nm
                  [ bgroup
                       (show m)
                       [ bench "intro" $ nf (Intro.select m) vc
                       -- , bench "median" $ nf (Median.select m) vc
                       , bench "quick" $ nf (Quick.select m) vc]
                  | m <- [0, n `div` 2, n - 1] ]
             | (nm,vc) <- lazyZip ["rnd", "asc", "dcs"] xs ]

lazyZip :: [a] -> [b] -> [(a,b)]
lazyZip (x:xs) ~(y:ys) = (x,y) : lazyZip xs ys
lazyZip [] _ = []


main :: IO ()
main = defaultMain (map benchAtSize [100000])
