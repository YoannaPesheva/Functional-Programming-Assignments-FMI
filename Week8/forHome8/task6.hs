main :: IO ()
main = do
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True

isPrimeG :: Int -> Bool
isPrimeG x
    | x < 2 = False
    | otherwise = helper 2
     where
        helper :: Int -> Bool
        helper divisor
            | divisor >= x = True
            | mod x divisor == 0 = False
            | otherwise = helper (divisor + 1)

isPrimeLC :: Int -> Bool
isPrimeLC x = null [divisor | divisor <- [2 .. (x - 1)], mod x divisor == 0] && x >= 2