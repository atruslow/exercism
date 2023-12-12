module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

getPlanetAge:: Planet -> Float
getPlanetAge Mercury = 0.2408467
getPlanetAge Venus = 0.61519726
getPlanetAge Earth = 1.0
getPlanetAge Mars = 1.8808158
getPlanetAge Jupiter = 11.862615
getPlanetAge Saturn = 29.447498
getPlanetAge Uranus = 84.016846
getPlanetAge Neptune = 164.79132

earthYearSeconds = 31557600



ageOn :: Planet -> Float -> Float
ageOn planet seconds = (seconds / (getPlanetAge planet)) / earthYearSeconds
