-- Histogram
module Exercise3 where

import Data.List
  ( group,
    sort,
  )
import Data.Range
  ( inRange,
    (+=+),
  )

histogram :: [Integer] -> String
histogram ns =
  let counts = getCounts . filter (inRange $ 0 +=+ 9) $ ns
      len = maximum counts
      printLine n = map (\x -> if x >= n then '*' else ' ') counts
   in unlines $
        map printLine [len, (len - 1) .. 1] ++ [replicate 10 '=', ['0' .. '9']]
  where
    getCounts = map (subtract 1 . length) . group . sort . ([0 .. 9] ++)
