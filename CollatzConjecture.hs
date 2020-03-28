module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x
  | x < 1 = Nothing
  | otherwise = Just collcount
  where collcount = collatz' x 0
        collatz' x' n'
          | x' == 0 = 0
          | x' == 1 = n'
          | even x' = (collatz' $ x' `div` 2) (n' + 1)
          | otherwise = (collatz' $ (3*x') + 1) (n' + 1)
