main :: IO ()
main = do
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False


revNum :: Int -> Int
revNum x = helper 0 x
    where
        helper :: Int -> Int -> Int
        helper res 0 = res
        helper res leftover = helper (res * 10 + mod leftover 10) (div leftover 10)


isPalindrome :: Int -> Bool
isPalindrome x 
    | x < 0 = error "N has to be non-negative"
    | otherwise = x == revNum x