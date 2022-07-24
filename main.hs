import Data.Char

calculateScore :: [Char] -> Int
calculateScore l = undefined

processFrame :: ([Char], Int, Int) -> Bool -> (Int, Int, Int)
processFrame f@([o], spa, str) bonus = processOneThrow f bonus
processFrame (fl@[f,s], spa, str) False = undefined
--TODO

processTwoThrows :: ([Char], Int, Int) -> (Int, Int, Int)
processTwoThrows ([a, '/'], spa, str) 
 | isDigit a = (calculateTwoThrowsScore 10 (digitToInt a) spa str, 1, 0)
 --TODO
processTwoThrows ([a, b], spa, str)
 | isDigit a && isDigit b && sumThrow < 10 = (calculateTwoThrowsScore sumThrow firstThrow spa str,str,0)
 --TODO
 where
  sumThrow = firstThrow + digitToInt b 
  firstThrow = digitToInt a

processOneThrow :: ([Char], Int, Int) -> Bool -> (Int, Int, Int)
processOneThrow (['x'], spa, str) bonus = (calculateOneThrowScore 10 spa str bonus, str ,1)
processOneThrow ([n], spa, str) bonus
 | isDigit n = (calculateOneThrowScore (digitToInt n) spa str bonus, str, 0)
 -- TODO

calculateOneThrowScore :: Int -> Int -> Int -> Bool -> Int
calculateOneThrowScore scr spa str bonus = ((boolToInt (not bonus)) + spa + str) * scr

calculateTwoThrowsScore :: Int -> Int -> Int -> Int -> Int
calculateTwoThrowsScore sum first spa str = (1 + str) * sum + first * spa

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt _ = 1

decraseUntilZero :: Int -> Int
decraseUntilZero 0 = 0
decraseUntilZero n = n - 1

