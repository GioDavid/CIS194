{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x | x<0 = []
toDigits x = result ++ [modresult]
  where
    (divresult, modresult) = x `divMod` 10
    result = if divresult < 10
      then [divresult]
      else toDigits divresult

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherStartingFromTheLeft (reverse x))
  where
    doubleEveryOtherStartingFromTheLeft [] = []
    doubleEveryOtherStartingFromTheLeft [x] = [x]
    doubleEveryOtherStartingFromTheLeft [x, y] = [x, 2*y]
    doubleEveryOtherStartingFromTheLeft (x:y:z) = [x, 2*y]  ++ doubleEveryOtherStartingFromTheLeft z

sumDigits :: [Integer] -> Integer
sumDigits x = sumAllDigits x
  where
    sumAllDigits [] = 0
    sumAllDigits [x] = sum(toDigits(x))
    sumAllDigits [x,y] = sum(toDigits(x) ++ toDigits(y))
    sumAllDigits (x:y:z) = sum(toDigits(x) ++ toDigits(y)) + sumDigits z


validate :: Integer -> Bool
validate x = ((sumDigits(doubleEveryOther(toDigits(x)))) `mod` 10) == 0
