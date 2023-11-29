import Data.List (sort)
main :: IO ()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False

hasIncDigits :: Int -> Bool
hasIncDigits x = x == read (sort (show x))

