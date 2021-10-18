module ValidatingCreditCardNumbers
  ( toDigits
  , doubleEveryOther
  , sumDigits
  , validate
  ) where

-- exercise 1
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n > 9     = let (q, r) = quotRem n 10 in r : toDigitsRev q
              | n > 0     = [n]
              | otherwise = []

-- exercise 2 - solution 1
-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther = reverse . doubleEveryOtherRev . reverse

-- doubleEveryOtherRev :: [Integer] -> [Integer]
-- doubleEveryOtherRev (x0:x1:xs) = x0 : 2 * x1 : doubleEveryOtherRev xs
-- doubleEveryOtherRev numbers = numbers

-- exercise 2 - solution 2
-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther numbers = fst $ foldr (\x (acc, bool) -> ((if bool then 2 * x else x) : acc, not bool)) ([], False) numbers

-- exercise 2 - solution 3
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther numbers@(x : xs)
  | even . length $ numbers = doubleEveryOtherRev numbers
  | otherwise               = x : doubleEveryOtherRev xs
doubleEveryOther numbers = numbers

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev = mapCyclic [(* 2), id]

mapCyclic :: [a -> b] -> [a] -> [b]
mapCyclic = zipWith id . cycle

-- exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- exercise 4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
