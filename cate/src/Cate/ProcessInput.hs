module Cate.ProcessInput where

import Cate.ANSICodes
import Cate.ASCIICodes
import Cate.TerminalSize
import Cate.Editor

import Data.Char (chr)

-- convert bytes to String
bytesToString = map chr

processInput :: [Int] -> Editor -> Editor
processInput byteList editor@(Editor _ (TerminalWindowSize width height) _ _ _ )
    | input == cursorRight = moveDisplayStartPosition $ moveInsertPositionHorizontal 1 editor
    | input == cursorLeft = moveDisplayStartPosition $ moveInsertPositionHorizontal (-1) editor
    | input == cursorDown = moveDisplayStartPosition $ moveInsertPositionHorizontal width editor
    | input == cursorUp = moveDisplayStartPosition $ moveInsertPositionHorizontal (-width) editor
    | input == [delete] = deleteFromEditor editor
    | otherwise = moveDisplayStartPosition $ insertIntoEditor input editor
    where
        input = bytesToString byteList

-- ascii
-- (show input)

-- bytes
-- (show byteList)

stopProcessingInput :: [Int] -> Bool
stopProcessingInput byteList
    | [escape] == input = True
    | otherwise = False
    where
        input = bytesToString byteList