main :: IO ()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

sumOfDivisors :: Int -> Int -> Int
sumOfDivisors x iter
    | iter == 0 = 0
    | mod x iter == 0 = iter + sumOfDivisors x (iter - 1)
    | otherwise = sumOfDivisors x (iter - 1)

areAmicable :: Int -> Int -> Bool
areAmicable x y = sumOfDivisors x x == sumOfDivisors y y