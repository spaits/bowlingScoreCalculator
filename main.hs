import Data.Char

calculateScore :: [Char] -> Int
calculateScore l = processFrames (splitOn l ' ') (0, 0, 10)

processFrames :: [[Char]] -> (Int, Int, Int) -> Int
processFrames [] _ = 0
processFrames (x:xs) (spa, str, n) = (fst4 last) + (processFrames xs (tripleFromLastThree last))
 where
  last = processFrame (x, spa, str, n) 

processFrame :: ([Char], Int, Int, Int) -> (Int, Int, Int, Int)
processFrame f@([o], spa, str, n) = processOneThrow f
processFrame fl@([f,s], spa, str, n) = processTwoThrows fl
processFrame (s, _, _, _) = error "Invalid frame: a frame can have two throws at maximum!"

processTwoThrows :: ([Char], Int, Int, Int) -> (Int, Int, Int, Int)
processTwoThrows ([a, '/'], spa, str, n) 
 | isDigit a = (calculateTwoThrowsScore 10 (digitToInt a) spa str, 1, 0, decraseUntilZero n)
 | otherwise = error "Error in processTwoThrows: A throw has to be a number!"
processTwoThrows ([a, b], spa, str, n)
 | isDigit a && isDigit b && sumThrow < 10 = (calculateTwoThrowsScore sumThrow firstThrow spa str, str, 0, decraseUntilZero n)
 | otherwise = error "Error in processTwoThrows: A throw has to be a number!"
 where
  sumThrow = firstThrow + digitToInt b 
  firstThrow = digitToInt a
processTwoThrows _ = error "Error in processTwoThrows: A throw has to be a number!"

processOneThrow :: ([Char], Int, Int, Int) -> (Int, Int, Int, Int)
processOneThrow (['x'], spa, str, n)  = (calculateOneThrowScore 10 spa str n, str , signum n, decraseUntilZero n)
processOneThrow ([o], spa, str, 0) 
 | isDigit o = (calculateOneThrowScore (digitToInt o) spa str 0, str, 0, 0)
 | otherwise = error "Error in processOneThrow: A throw has to be a number"
processOneThrow _ = error "Error in processOneThrow: You can have single throw only in the bonus rounds!"

calculateOneThrowScore :: Int -> Int -> Int -> Int -> Int
calculateOneThrowScore scr spa str n = ((signum n) + spa + str) * scr

calculateTwoThrowsScore :: Int -> Int -> Int -> Int -> Int
calculateTwoThrowsScore sum first spa str = (1 + str) * sum + first * spa

decraseUntilZero :: Int -> Int
decraseUntilZero 0 = 0
decraseUntilZero n = n - 1

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

tripleFromLastThree :: (a, b, c, d) -> (b, c, d)
tripleFromLastThree (_, x, y, z) = (x, y, z)

splitOn :: Eq a => [a] -> a -> [[a]]
splitOn [] _ = [[]]
splitOn (x:xs) c
 | x /= c = [[x] ++ recRes!!0] ++ (drop 1 recRes)
 | otherwise = [[]] ++ recRes
 where
  recRes = splitOn xs c
