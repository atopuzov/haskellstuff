{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where
import Log


decodeMessage :: [[Char]] -> LogMessage
decodeMessage ("I" :       time : msg) = LogMessage Info (read time) (unwords msg)
decodeMessage ("E" : lvl : time : msg) = LogMessage (Error (read lvl)) (read time) (unwords msg)
decodeMessage ("W" :       time : msg) = LogMessage Warning (read time) (unwords msg)
decodeMessage msg = Unknown (unwords msg)

parseMessage :: String -> LogMessage
parseMessage = decodeMessage . words

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert x Leaf = Node Leaf x Leaf
insert x@((LogMessage _ time1 _)) (Node lmt y@(LogMessage _ time2 _) rmt) =
  if time1 < time2
  then Node (insert x lmt) y rmt
  else Node lmt y (insert x rmt)
insert _ mt = mt

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lmt lm rmt) = inOrder lmt ++ [lm] ++ inOrder rmt

errorSev50andAbove :: LogMessage -> Bool
errorSev50andAbove (LogMessage (Error lvl) _ _) = lvl >= 50
errorSev50andAbove _ = False

getErrorMsg :: LogMessage -> String
getErrorMsg (LogMessage (Error _) _ msg) = msg
getErrorMsg _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map getErrorMsg (filter errorSev50andAbove logs)


main :: IO ()
main = do
  putStrLn . show $ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
  putStrLn . show $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
  putStrLn . show $ parseMessage "This is not in the right format" == Unknown "This is not in the right format"
