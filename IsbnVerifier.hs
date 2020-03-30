module IsbnVerifier (isbn) where
import qualified Data.Char as Ch

isbn :: String -> Bool
isbn cur
  | length cleansed /= 10 = False
  | not $ all Ch.isNumber mostcur = False
  | not (lastchar == 'X' || Ch.isNumber lastchar) = False
  | otherwise = mod (foldr (+) 0 $ (zipWith (*) (map Ch.digitToInt mostcur) $ reverse [2 .. 10]) ++ [lastval]) 11 == 0
  where
    cleansed = filter (\x -> Ch.isNumber x || x == 'X') cur
    mostcur = init cleansed
    lastchar = last cleansed
    lastval
      | lastchar == 'X' = 10
      | Ch.isNumber lastchar = Ch.digitToInt lastchar
      | otherwise = 0
    
    
    
