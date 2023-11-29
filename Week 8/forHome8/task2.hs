main :: IO ()
main = do
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13

    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13

myGcdG :: Int -> Int -> Int
myGcdG x y
 | y == 0 = x
 | x == 0 = y
 | otherwise = myGcdG y (mod x y)

myGcdPM :: Int -> Int -> Int
myGcdPM 0 y = y -- catches when x is 0, y is whatever
myGcdPM x 0 = x -- catches when y is 0, x is whatever
myGcdPM x y = myGcdPM y (mod x y) -- otherwise