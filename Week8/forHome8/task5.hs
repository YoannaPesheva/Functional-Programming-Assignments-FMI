main :: IO ()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False

hasIncDigits :: Int -> Bool
hasIncDigits x = isBigger x
  where
    isBigger :: Int -> Bool
    isBigger leftover
      | leftover < 10 = True -- so if theres only one digit left - base case
      | mod leftover 10 < mod (div leftover 10) 10 = False -- checks if the digit after is < previous
      | otherwise = isBigger (div leftover 10)
