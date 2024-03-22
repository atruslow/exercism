module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock
  { clockHours :: Int
  , clockMinutes :: Int
  }
  deriving (Eq, Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock ((hour + carryOver) `mod` 24) (min `mod` 60)
 where
  carryOver = min `div` 60 

toString :: Clock -> String
toString clock = concat [hourString, ":", minuteString]
 where
  hourString = if hours > 9 then show hours else "0" ++ show hours
  minuteString = if minutes > 9 then show minutes else "0" ++ show minutes
  hours = clockHours clock
  minutes = clockMinutes clock

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min clock = Clock newHours newMin
 where
  carryOver = totalMin `div` 60
  totalMin = min + clockMinutes clock
  newHours = (carryOver + hour + clockHours clock) `mod` 24
  newMin = totalMin `mod` 60
