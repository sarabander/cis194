{-# OPTIONS_GHC -Wall #-}
module Main where

import Scrabble
import Sized
import JoinList
-- import Editor
{-
main :: IO ()
main = runEditor editor $ reify $ fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
-}

reify :: JoinList (Score, Size) String -> JoinList (Score, Size) String
reify = id

initBuf :: JoinList (Score, Size) String 
initBuf = fromString "yks\nkaks\nkolm\n"
