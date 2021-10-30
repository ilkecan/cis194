-- Hopscotch
module Exercise1 where

skips :: [a] -> [[a]]
skips l = takeWhile (not . null) $ map (takeEvery l) [1 ..]

takeEvery :: [a] -> Int -> [a]
takeEvery list n = case drop (n - 1) list of
  (x : xs) -> x : takeEvery xs n
  [] -> []
