module Isogram (isIsogram) where
import qualified Data.Char as Ch


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [b | b <- xs, b <= x] ++ [x] ++ quicksort [c | c <- xs, c > x]
  
isIsogram :: String -> Bool
isIsogram xs =
  let xss = quicksort $ map (Ch.toLower) $ filter Ch.isAlpha xs
      compareto = unwords [tail xss,""]
  in and $ zipWith (/=) xss compareto
