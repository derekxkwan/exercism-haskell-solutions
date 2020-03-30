module Diamond (diamond) where
import qualified Data.Char as Ch

diamond :: Char -> Maybe [String]
diamond ltr
  | ltr < 'A' || ltr > 'Z' = Nothing
  | otherwise = Just $ map (getStr) $ getRange ltr
  where
    getRange ch = ['A' .. ch] ++ (tail $ reverse ['A' .. ch])
    curStep cur = Ch.ord cur - Ch.ord 'A'
    numSpaces = 1 + (curStep ltr)*2
    midPoint = quot numSpaces 2
    getPlacement cur
      | cur <= 'A' = [midPoint]
      | otherwise = [(+ curStep cur),(subtract $ curStep cur)] <*> [midPoint]
    getStr cur = map (\x -> if x then cur else ' ') $ map (\x -> elem x $ getPlacement cur) [0 .. (numSpaces - 1)]
