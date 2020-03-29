module Grains (square, total) where
import qualified Data.Bits as DB

square :: Integer -> Maybe Integer
square n
  | n <= 0 || n > 64 = Nothing
  | otherwise = Just $ DB.shift 2 $ (fromInteger n) - 2



-- 2^k sum 0 to n = 2^(n+1) - 1
total :: Integer
total = DB.shift 2 63 - 1
