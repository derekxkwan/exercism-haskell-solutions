module RunLength (decode, encode) where
import qualified Data.Char as Ch

decode :: String -> String
decode encodedText
  | encodedText == "" = ""
  | otherwise = let (numstr, (x:xs)) = break (not . Ch.isNumber) encodedText
                in case numstr of
                     "" -> x:[] ++ decode xs
                     _ -> (replicate (read numstr) x) ++ decode xs

encode :: String -> String
encode text
  | text == "" = text
  | otherwise = let wantchar = head text
                    (x,xs) = break (/= wantchar) text
                    runlength = length x
                in case runlength of
                     1 -> wantchar:[] ++ encode xs
                     _ -> (show runlength) ++ wantchar:[] ++ encode xs

