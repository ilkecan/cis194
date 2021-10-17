module ValidatingCreditCardNumbers where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n > 0     = let (q, r) = quotRem n 10 in r : toDigitsRev q
              | otherwise = []
