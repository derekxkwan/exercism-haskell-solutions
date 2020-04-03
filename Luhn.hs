module Luhn (isValid) where
import qualified Data.Char as Ch

isValid :: String -> Bool
isValid n
  | length nfilt <= 1 = False
  | otherwise = mod (foldr (+) 0 $ ndoubled) 10 == 0
  where
    nfilt = filter Ch.isNumber n
    digitDoubler x = if x2 >= 10 then x2 - 9 else x2 where x2 = 2*x
    ndoubled = map(\x -> let (a,b) = x; b2 = Ch.digitToInt b; in if odd a then digitDoubler b2 else b2) $ zip ([0.. ] :: [Int]) (reverse $ nfilt)
