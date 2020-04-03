module Yacht (yacht, Category(..)) where

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <-xs, y >= x]

yacht :: Category -> [Int] -> Int
yacht category dice = case category of
  Ones -> length $ filter (==1) dice
  Twos -> 2 * (length $ filter (==2) dice)
  Threes -> 3 * (length $ filter (==3) dice)
  Fours -> 4 * (length $ filter (== 4) dice)
  Fives -> 5 * (length $ filter (== 5) dice)
  Sixes -> 6 * (length $ filter (== 6) dice)
  Choice -> foldr (+) 0 dice
  LittleStraight -> if sortdice == [1..5] then 30 else 0
  BigStraight -> if sortdice == [2..6] then 30 else 0
  Yacht -> if all (== (head dice)) dice then 50 else 0
  FourOfAKind -> let wantval = sortdice!!2 in if length (filter (== wantval) dice) >= 4 then 4*wantval else 0
  FullHouse -> if and [all (== head adice) adice, all (== head bdice) bdice, or [dicelen == (3,2), dicelen == (2,3)]] then foldr (+) 0 dice else 0
  where
    sortdice = quicksort dice
    (adice, bdice) = break (/= head sortdice) sortdice
    dicelen = (length adice, length bdice)

    
