module Acronym (abbreviate) where
import qualified Data.Char as Ch

unCamel :: String -> [String]
unCamel xs@(y:ys)
  | all Ch.isUpper ys || all Ch.isLower ys || length ys <= 1 = xs:[]
  | otherwise = (y:z):(unCamel z2)
      where (z,z2) = break Ch.isUpper ys

punctStrip :: String -> String
punctStrip = map (\x -> if Ch.isPunctuation x then ' ' else x)

possessStrip :: String -> String
possessStrip = filter (/='\'')

abbreviate :: String -> String
abbreviate = map (Ch.toUpper) . map (head) . concatMap unCamel . words . punctStrip . possessStrip

