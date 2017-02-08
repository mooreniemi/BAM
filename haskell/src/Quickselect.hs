module Quickselect ( sortselect
                   , quickselect
                   ) where

import qualified Data.Vector.Algorithms.Intro as I
import           Data.Vector.Unboxed          (Vector)
import qualified Data.Vector.Unboxed          as V

sortselect :: Int -> Vector Int -> Maybe Int
sortselect k = (V.!? (k-1)) . V.modify I.sort

quickselect :: Int -> Vector Int -> Maybe Int
quickselect k xs = Just k
