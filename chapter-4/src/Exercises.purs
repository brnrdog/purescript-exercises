module Exercises where

import Prelude
import Data.Array (filter, head, tail)
import Data.Maybe (fromMaybe)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x = isEven (mod x 2)

evenNumbers :: Array Int -> Int
evenNumbers [x] =
  if isEven x
  then 1
  else 0
evenNumbers arr =
  evenNumbers [(fromMaybe 0 (head arr))] +
  evenNumbers (fromMaybe [0] (tail arr))

squareNumbers :: Array Int -> Array Int
squareNumbers arr = (\n -> n * n) <$> arr

onlyPositives :: Array Int -> Array Int
onlyPositives arr = filter (\n -> n >= 0) arr
