{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.Char
-- import Helpers

-- Ex. 1

parseMessage :: String -> LogMessage
parseMessage message = dissect (words message) where
  dissect ("E":severity:body)
    | isInt severity = parseBody (Error (read severity)) body
  dissect ("I":body) = parseBody Info body
  dissect ("W":body) = parseBody Warning body
  dissect _ = Unknown message

  parseBody msgType (time:text)
    | isInt time && not (null text) =
        LogMessage msgType (read time) (unwords text)
  parseBody _ _ = Unknown message

isInt :: String -> Bool
isInt = all isDigit

parse :: String -> [LogMessage]
parse = map parseMessage . lines

--testParse parse 10 "error.log"

-- Ex. 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newMessage Leaf = Node Leaf newMessage Leaf
insert newMessage (Node leftTree oldMessage rightTree) =
  let (LogMessage _ newTimeStamp _) = newMessage
      (LogMessage _ oldTimeStamp _) = oldMessage in
   if newTimeStamp < oldTimeStamp
   then (Node (insert newMessage leftTree) oldMessage rightTree)
   else (Node leftTree oldMessage (insert newMessage rightTree))
                        
-- Ex. 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Ex. 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMessage rightTree) =
  inOrder leftTree ++ [logMessage] ++ inOrder rightTree

-- Ex. 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map justMsg . inOrder . build . filter bigError where
  bigError (LogMessage (Error severity) _ _) = severity >= 50
  bigError _ = False
  justMsg = \(LogMessage _ _ msg) -> msg
                                                  
--testWhatWentWrong parse whatWentWrong "sample.log"
--testParse parse 20 "sample.log" >>= sequence . map print . inOrder . build 

main :: IO ()
main = testParse parse 20 "sample.log" >>= mapM_ print . inOrder . build 
