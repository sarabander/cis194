module Helpers where

printList :: Show a => [a] -> IO ()
printList [] = putStr ""
printList (move:moves) =
  do putStr (show move ++ " ")
     _ <- getLine
     printList moves
