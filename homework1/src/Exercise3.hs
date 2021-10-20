module Exercise3 where

import Exercise1 (toDigits)

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)
