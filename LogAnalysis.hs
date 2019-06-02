{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Maybe
import Text.Read
import Data.List.Split


parse :: String -> [LogMessage]
parse = map parseMessage . splitOn "\n" 

tryParse :: String -> Maybe LogMessage
tryParse line = 
    case words line of
        "I":ts -> makeMessage Info ts
        "W":ts -> makeMessage Warning ts
        "E":level:xs -> do
            nLevel <- readMaybe level
            makeMessage (Error nLevel) xs
        _  -> Nothing

parseMessage :: String -> LogMessage
parseMessage line = fromMaybe (Unknown line) (tryParse line)

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = [] 
intersperse _ [x] = x
intersperse sep (x:xs) = x ++ [sep] ++ intersperse sep xs

makeMessage :: MessageType -> [String] -> Maybe LogMessage
makeMessage messageType (timestamp:lineTail) = do
    time <- readMaybe timestamp
    return $ LogMessage messageType time message
    where message = intersperse ' ' lineTail
makeMessage _ _ = Nothing

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ time _) (Node leftNode treeMsg@(LogMessage _ treeTime _) rightNode) |
    time <= treeTime = Node (insert msg leftNode) treeMsg rightNode |
    otherwise = Node leftNode treeMsg (insert msg rightNode)
insert _ (Node _ (Unknown _) _ ) = error "All messages in tree should have timestamp"


build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg:msgs) = insert msg $ build msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map msgStr sortedMsgs
    where sortedMsgs = filter (\x -> errorLevel x >= 50) . inOrder $ build msgs
          msgStr (LogMessage _  _ msg) = msg
          msgStr (Unknown _) = error "Message should not be unknown"
          errorLevel (LogMessage (Error e) _ _) = e
          errorLevel (LogMessage _ _ _) = 0
          errorLevel (Unknown _) = error "Message should not be unknown"
{- 
parseMessage line = ==

splitByWord "" = []
splitByWord x = w : spliter x

spliter (x:xs) = case x of
    ' '   -> ("", xs)
    other -> (x : spliter xs, xs)

parseMessageType 'I' = Just Info
parseMessageType 'W' = Just Warning
parseMessageType 'I' = Just Error 
-}
