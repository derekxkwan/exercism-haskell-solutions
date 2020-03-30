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
  in ((fromEnum x)*10 + fromEnum y)*(10^(fromEnum z)) 
