module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify num
  | num < 1 = Nothing
  | otherwise = case compare num sumDiv of
                  EQ -> Just Perfect
                  GT -> Just Deficient
                  LT -> Just Abundant
                  where
                    sumDiv = foldr (+) 0 $ filter (\x -> rem num x == 0) [1 .. (num - 1)]
