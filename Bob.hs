module Bob (responseFor) where
import qualified Data.Char as Ch

responseFor :: String -> String
responseFor xs
  | is_blank xss = "Fine. Be that way!"
  | is_yelling xss && is_question xs = "Calm down, I know what I'm doing!"
  | is_yelling xss = "Whoa, chill out!"
  | is_question xss = "Sure."
  | otherwise = "Whatever."
  where
    is_yelling xs = any Ch.isAlpha xs && (all Ch.isAsciiUpper $ filter Ch.isAlpha xs)
    is_question xs = last xs == '?'
    is_blank xs = all Ch.isSpace xs
    xss = dropWhile Ch.isSpace $ reverse $ dropWhile Ch.isSpace $ reverse xs

   
