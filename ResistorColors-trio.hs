module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show


colorVal :: Color -> Int
colorVal x = case x of
               Black -> 0
               Brown -> 1
               Red -> 2
               Orange -> 3
               Yellow -> 4
               Green -> 5
               Blue -> 6
               Violet -> 7
               Grey -> 8
               White -> 9

-- output "x ohms" or kiloohms or megaohms
label :: Resistor -> String
label resistor
  | ohm < 1000 = (show ohm) ++ " ohms"
  | ohm < 10^6 = (show $ quot ohm 1000) ++ " kiloohms"
  | ohm < 10^9 = (show $ quot ohm $ 10^6) ++ " megaohms"
  | otherwise = (show $ quot ohm $ 10^9) ++ " gigaohms"
  where
    ohm = ohms resistor
    
-- output ohms as int
ohms :: Resistor -> Int
ohms resistor =
  let (x,y,z) = bands resistor
  in ((colorVal x)*10 + colorVal y)*(10^(colorVal z)) 
