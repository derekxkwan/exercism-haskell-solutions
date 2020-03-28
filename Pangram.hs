module Pangram (isPangram) where
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = (== 0) $ length $ filter (\x -> not $ elem x (map toLower text)) "abcdefghijklmnopqrstuvwxyz"
