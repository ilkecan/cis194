module Exercise4 where

import Exercise1 (toDigits)
import Exercise2 (doubleEveryOther)
import Exercise3 (sumDigits)

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
