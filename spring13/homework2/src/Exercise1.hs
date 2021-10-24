module Exercise1 where

import Log
  ( LogMessage (LogMessage, Unknown),
    MessageType (Error, Info, Warning),
  )

-- TODO: handle `parseMessage "E This is not in the right format"`
parseMessage :: String -> LogMessage
parseMessage m = case words m of
  "I" : timeStamp : string -> LogMessage Info (read timeStamp) (unwords string)
  "W" : timeStamp : string ->
    LogMessage Warning (read timeStamp) (unwords string)
  "E" : severity : timeStamp : string ->
    LogMessage (Error (read severity)) (read timeStamp) (unwords string)
  _unknownMessage -> Unknown m

parse :: String -> [LogMessage]
parse = map parseMessage . lines
