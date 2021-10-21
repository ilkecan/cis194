module Exercise2 where

doubleEveryOtherRev1 :: [Integer] -> [Integer]
doubleEveryOtherRev1 (x0 : x1 : xs) = x0 : 2 * x1 : doubleEveryOtherRev1 xs
doubleEveryOtherRev1 numbers = numbers

--
doubleEveryOtherRev2 :: [Integer] -> [Integer]
doubleEveryOtherRev2 = mapCyclic [id, (* 2)]

mapCyclic :: [a -> b] -> [a] -> [b]
mapCyclic = zipWith id . cycle

--
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev = doubleEveryOtherRev1

--
--

doubleEveryOther1 :: [Integer] -> [Integer]
doubleEveryOther1 = reverse . doubleEveryOtherRev1 . reverse

--
doubleEveryOther2 :: [Integer] -> [Integer]
doubleEveryOther2 numbers = fst $ foldr (\x (acc, bool) -> ((if bool then 2 * x else x) : acc, not bool)) ([], False) numbers

--
doubleEveryOther3 :: [Integer] -> [Integer]
doubleEveryOther3 numbers@(x : xs)
  | even . length $ numbers = 2 * x : doubleEveryOtherRev xs
  | otherwise = doubleEveryOtherRev numbers
doubleEveryOther3 numbers = numbers

--
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = doubleEveryOther3
