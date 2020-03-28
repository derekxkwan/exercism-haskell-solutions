module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | all (`elem` "ACGT") xs = Right (M.fromList $ nucMap xs)
  | otherwise = Left (xs)
  where
    nucCtr nuc xs = length $ filter (==nuc) xs
    nucMap xs = [(A, nucCtr 'A' xs),
                 (C, nucCtr 'C' xs),
                 (G, nucCtr 'G' xs),
                 (T, nucCtr 'T' xs)
                ]

