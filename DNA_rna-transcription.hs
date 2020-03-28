module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs =
  let toRNA' 'G' = Right 'C'
      toRNA' 'C' = Right 'G'
      toRNA' 'T' = Right 'A'
      toRNA' 'A' = Right 'U'
      toRNA' x = Left x
  in mapM (toRNA') xs
