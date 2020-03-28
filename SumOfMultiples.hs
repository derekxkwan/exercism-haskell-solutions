module SumOfMultiples (sumOfMultiples) where
import qualified Data.List as L

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let multTo n x
        | x > 0 = takeWhile (< n) $ map (*x) [1 ..]
        | otherwise = [0]
  in foldr (+) 0 $ L.nub $ factors >>= multTo limit

