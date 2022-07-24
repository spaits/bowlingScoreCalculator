import Data.Char

calculateScore :: [Char] -> Int
calculateScore l = undefined

processFrame :: ([Char], Int, Int) -> Bool -> (Int, Int, Int)
processFrame (fl@[o], spa, str) bonus = processOneThrow (o, spa, str) bonus
processFrame (fl@[f,s], spa, str) bonus = undefined

processOneThrow :: (Char, Int, Int) -> Bool -> (Int, Int, Int)
processOneThrow ('x', spa, str) bonus = (calculateOneThrowScore 10 spa str bonus, decraseUntilZero spa,(decraseUntilZero str) + 1)
processOneThrow (n, spa, str) bonus
 | isDigit n = (calculateOneThrowScore (digitToInt n) spa str bonus, decraseUntilZero spa, decraseUntilZero str)
 -- TODO

calculateOneThrowScore :: Int -> Int -> Int -> Bool -> Int
calculateOneThrowScore scr spa str bonus = ((boolToInt (not bonus)) + spa + str) * scr

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt _ = 1

decraseUntilZero :: Int -> Int
decraseUntilZero 0 = 0
decraseUntilZero n = n - 1

