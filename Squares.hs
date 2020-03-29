module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = (foldr (+) 0 [1 .. n])^2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = (n * (n + 1) * (1 + 2*n)) `quot` 6
