module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds =
  case planet of
    Mercury -> yrs/0.2408467
    Venus -> yrs/0.61519726
    Earth -> yrs
    Jupiter -> yrs/11.86261
    Saturn -> yrs/29.447498
    Uranus -> yrs/ 84.016846
    Neptune -> yrs/164.79132
    Mars -> yrs/1.8808158
    where yrs = seconds/31557600.0
