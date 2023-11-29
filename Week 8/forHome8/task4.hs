main :: IO ()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

sumOfDivisors :: Int -> Int
sumOfDivisors x = sum [divisor | divisor <- [1 .. x], mod x divisor == 0]

areAmicable :: Int -> Int -> Bool
areAmicable x y = sumOfDivisors x == sumOfDivisors y