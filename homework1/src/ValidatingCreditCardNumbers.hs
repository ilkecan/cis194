module ValidatingCreditCardNumbers where

-- exercise 1
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n > 0     = let (q, r) = quotRem n 10 in r : toDigitsRev q
              | otherwise = []

-- exercise 2 - solution 1
-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther = reverse . doubleEveryOtherRev . reverse

-- doubleEveryOtherRev :: [Integer] -> [Integer]
-- doubleEveryOtherRev (x0:x1:xs) = x0 : 2 * x1 : doubleEveryOtherRev xs
-- doubleEveryOtherRev list = list

-- exercise 2 - solution 2
-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther list = fst $ foldr (\x (acc, bool) -> ((if bool then 2 * x else x) : acc, not bool)) ([], False) list

-- exercise 2 - solution 3
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list@(x : xs) | even . length $ list = doubleEveryOtherRev list
                               | otherwise = x : doubleEveryOtherRev xs
doubleEveryOther list = list

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev = mapCyclic [(* 2), id]

mapCyclic :: [a -> b] -> [a] -> [b]
mapCyclic = zipWith id . cycle
