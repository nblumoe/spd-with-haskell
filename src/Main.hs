module Main where

-- | Determine if string has length 5
--
-- Examples:
--
-- >>> hasLengthFive "abc"
-- False
--
-- >>> hasLengthFive "123 5"
-- True
hasLengthFive :: String -> Bool
hasLengthFive s = length s == 5

-- | Calculate cartesian distance between two points
--
-- Examples:
--
-- >>> distance (0, 0) (1, 0)
-- 1.0
--
-- >>> distance (0, 0) (1, 1)
-- 1.414...
--
-- >>> distance (1, 2) (3, 6)
-- 4.472...
--
distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

main :: IO ()
main = do
  putStrLn "hello world"

-- HtDD L5
-- Design a data definition to represent the current state of a New Year's Eve countdown

-- Countdown is one of:
--  - "not yet started"
--  - Number[1, 10]
--  - "complete"
-- interp. a countdown with start and end states

c1 = "not yet started"
c2 = 10
c3 = 1
c4 = "complete"

data Countdown = NotStarted | Completed | Count Int

fnForCountdown :: Countdown -> t
fnForCountdown c = case c of
  NotStarted -> undefined
  Completed -> undefined
  Count c -> undefined

-- HtDD L8
-- Given the data definition for Countdown, design a function that produces an image of the current
-- state.

-- | Produce visual representation of a countdown
--
-- Examples:
--
-- >>> renderCountdown NotStarted
-- "-----\n| > |\n-----"
--
-- >>> renderCountdown Completed
-- "------\n| :D |\n------"
--
-- >>> renderCountdown $ Count 10
-- "------\n| 10 |\n------"
--
-- >>> renderCountdown $ Count 3
-- "-----\n| 3 |\n-----"

renderCountdown :: Countdown -> String
renderCountdown c =
  "----" ++ additionalDashes ++ "\n| " ++ counter ++ " |\n----" ++ additionalDashes
  where counter = case c of
          NotStarted -> ">"
          Completed -> ":D"
          Count c -> show c
        additionalDashes = take (length counter) $ repeat '-'

-- HtDD P3
-- Design a data definition and a function for a program to track a rocket's return to Earth.

-- RocketHeight is Integer(0, 100]
-- Interp. Landed if rocket's descent has ended, otherwise height in km

rh1 = Height 100
rh2 = Height 13
rh3 = Landed

data RocketHeight = Landed | Height Int

fnForRocketHeight :: RocketHeight -> t
fnForRocketHeight rh
  | Landed <- rh                    = undefined
  | Height h <- rh, h > 0, h <= 100 = undefined rh

-- | Render remaining rocket height as a tweet
--
-- Examples:
--
-- >>> renderRocketHeight $ Height 100
-- "Current rocket height: 100km #awesomerocket"
--
-- >>> renderRocketHeight $ Height 12
-- "Current rocket height: 12km #awesomerocket"
--
-- >>> renderRocketHeight Landed
-- "The rocket has landed!"

renderRocketHeight :: RocketHeight -> String
renderRocketHeight rh
  | Landed   <- rh                 = "The rocket has landed!"
  | Height h <- rh, h > 0, h < 100 = "Current rocket height: " ++ show h ++ "km #awesomerocket"
