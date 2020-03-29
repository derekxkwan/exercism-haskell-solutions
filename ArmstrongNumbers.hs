module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong x
  | x < 0 = False
  | otherwise = x2 == sumOfPowers x2
  where
    x2 = fromIntegral x
    intToDigits = map (\y -> read [y] :: Int) . show
    numDigits = length . intToDigits
    powerDigits z = map (^ (numDigits z)) $ intToDigits z
    sumOfPowers = foldr (+) 0 . powerDigits
