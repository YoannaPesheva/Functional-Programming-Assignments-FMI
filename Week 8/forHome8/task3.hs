main :: IO ()
main = do
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False

revNum :: Int -> Int
revNum x = read (reverse (show x))


isPalindrome :: Int -> Bool
isPalindrome x 
 | x < 0 = error "N has to be non-negative"
 | otherwise = x == revNum x