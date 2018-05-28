module Main (main) where

import           Test.QuickCheck

import           Data.List                  (sort)
import qualified Data.Vector                as Vector
import qualified Data.Vector.Unboxed        as UVector

import           Data.Select.Vector
import qualified Data.Select.Vector.Unboxed as U

main :: IO ()
main =
    quickCheck $
    do NonEmpty xs <- arbitrary :: Gen (NonEmptyList Int)
       i <- choose (0, length xs - 1)
       let ys = sort xs
       pure $
           (ys !! i) === select i (Vector.fromList xs) .&&. (ys !! i) ===
           U.select i (UVector.fromList xs)
