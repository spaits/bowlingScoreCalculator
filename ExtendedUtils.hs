module ExtendedUtils (decraseUntilZero, fst4, tripleFromLastThree, splitOn) where

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
