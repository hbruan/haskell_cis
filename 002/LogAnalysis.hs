module LogAnalysis where

import Log

-- exercise 1
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
  ("E":level:ts:msg) -> LogMessage (Error (read level)) (read ts) (unwords msg)
  ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse p = [parseMessage l | l <- lines p]

-- test: testParse parse 10 "error.log"

-- exercise 2 3 4

insert :: LogMessage -> MessageTree -> MessageTree

insert log Leaf = Node Leaf log Leaf
insert (Unknown _) node = node
insert log@(LogMessage _ lts _) node@(Node left center@(LogMessage _ cts _) right) = Node (insert log left) center right
insert log (Node left center right) = Node left center (insert log right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [m] = Node Leaf m Leaf
build (m:ms) = insert m (build ms)

inorder :: MessageTree -> [LogMessage]
inorder Leaf = []
inorder (Node Leaf center Leaf) = [center]
inorder (Node left center right) = inorder left ++ [center] ++ inorder right

-- exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = [msg | (LogMessage _ _ msg) <- inorder (build errors)] where errors = [message | message@(LogMessage (Error level) _ _) <- messages, level > 50]