module Exercises where
  
import Prelude

import Control.MonadZero (guard)
import Data.Array ((..), filter)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

isEven :: Int -> Boolean
isEven 0 = true
isEven -1 = false
isEven n = isEven $ n - 2

numberOfEvens :: Array Int -> Int
numberOfEvens [] = 0
numberOfEvens arr = if isEven x then 1 + numberOfEvens xs else numberOfEvens xs
  where x = unsafePartial $ head arr
        xs = unsafePartial $ tail arr

infix 8 filter as <$?>

squares :: Array Int -> Array Int
squares = map (\x -> x * x)

positives :: Array Int -> Array Int
positives xs = (\x -> x >= 0) <$?> xs

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1..n
  j <- i..n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime x = factors x == pure [1, x]