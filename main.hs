import Data.Char

calculateScore :: [Char] -> Int
calculateScore l = undefined

processFrames :: [[Char]] -> Int -> Int -> Int -> Int
processFrames [] _ _ _ = 0
processFrames (x:xs) spa str 0 = undefined
processFrames (x:xs) spa str n = undefined

processFrame :: ([Char], Int, Int, Int) -> (Int, Int, Int, Int)
processFrame f@([o], spa, str, n) = processOneThrow f
processFrame fl@([f,s], spa, str, n) = processTwoThrows fl
--TODO

processTwoThrows :: ([Char], Int, Int, Int) -> (Int, Int, Int, Int)
processTwoThrows ([a, '/'], spa, str, n) 
 | isDigit a = (calculateTwoThrowsScore 10 (digitToInt a) spa str, 1, 0, decraseUntilZero n)
 --TODO
processTwoThrows ([a, b], spa, str, n)
 | isDigit a && isDigit b && sumThrow < 10 = (calculateTwoThrowsScore sumThrow firstThrow spa str, str, 0, decraseUntilZero n)
 --TODO
 where
  sumThrow = firstThrow + digitToInt b 
  firstThrow = digitToInt a

processOneThrow :: ([Char], Int, Int, Int) -> (Int, Int, Int, Int)
processOneThrow (['x'], spa, str, n)  = (calculateOneThrowScore 10 spa str n, str , 1, decraseUntilZero n)
processOneThrow ([o], spa, str, n) 
 | isDigit o = (calculateOneThrowScore (digitToInt o) spa str n, str, 0, decraseUntilZero n)
 -- TODO

calculateOneThrowScore :: Int -> Int -> Int -> Int -> Int
calculateOneThrowScore scr spa str n = ((signum n) + spa + str) * scr

calculateTwoThrowsScore :: Int -> Int -> Int -> Int -> Int
calculateTwoThrowsScore sum first spa str = (1 + str) * sum + first * spa

decraseUntilZero :: Int -> Int
decraseUntilZero 0 = 0
decraseUntilZero n = n - 1

