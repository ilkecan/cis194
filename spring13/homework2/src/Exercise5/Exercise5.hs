module Exercise5 where

import Exercise3 (build)
import Exercise4 (inOrder)
import Log
  ( KnownLogMessage,
    LogMessage,
    MessageType (Error),
  )

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map (\(_, _, msg) -> msg) . filter (isSevereError 50) . inOrder . build

isSevereError :: Int -> KnownLogMessage -> Bool
isSevereError n (Error severity, _, _) = severity >= n
isSevereError _ _ = False
