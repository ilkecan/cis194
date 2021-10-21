module Exercise1 where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 9 = let (q, r) = quotRem n 10 in r : toDigitsRev q
  | n > 0 = [n]
  | otherwise = []
