main :: IO ()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True

isPerfect :: Int -> Bool
isPerfect x = x == sum [divisor | divisor <- [1 .. (x - 1)], mod x divisor == 0] && x > 1