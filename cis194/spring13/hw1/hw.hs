{-# OPTIONS_GHC -Wall #-}
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0  = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse pairedUp
  where
    f x b = if b then 2 * x else x
    pairedUp = zipWith f (reverse xs) (cycle [False, True])


sumDigits :: [Integer] -> Integer
sumDigits xs = sum . concat $ map toDigits xs

-- Monadic version
sumDigits' :: [Integer] -> Integer
sumDigits' xs = sum (xs >>= toDigits)
-- Point free version
-- sumDigits' = sum . flip (>>=) toDigits

validate :: Integer -> Bool
validate x = ((sumDigits $ doubleEveryOther $ toDigits x) `mod` 10) == 0

main :: IO ()
main = do
  putStrLn . show $ toDigits 1234 == [1,2,3,4]
  putStrLn . show $ toDigitsRev 1234 == [4,3,2,1]
  putStrLn . show $ toDigits 0 == []
  putStrLn . show $ toDigits (-17) == []
  putStrLn . show $ doubleEveryOther [8,7,6,5] == [16,7,12,5]
  putStrLn . show $ doubleEveryOther [1,2,3] == [1,4,3]
  putStrLn . show $ sumDigits [16,7,12,5] == 22
  putStrLn . show $ validate 4012888888881881 == True
  putStrLn . show $ validate 4012888888881882 == False
