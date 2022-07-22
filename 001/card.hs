toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs)
  | length (xs) `mod` 2 == 0 = x*2 : y : doubleEveryOther(xs)
  | otherwise = x : y*2 : doubleEveryOther(xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = sum(toDigits x)
sumDigits (x:xs) = sum(toDigits x)  + sumDigits(xs)

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0