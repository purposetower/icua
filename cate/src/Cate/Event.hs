module Cate.Event (processEvent, stopEvent) where

import Cate.ANSICodes
import Cate.ASCIICodes
import Cate.Display
import Cate.Types.EditorTypes (Editor (..))

import Data.Char (chr)

-- convert bytes to String
bytesToString = map chr

processEvent :: [Int] -> Editor -> Editor
processEvent byteList editor
    | input == cursorRight = moveStartPositionToShowLastCursor $ moveInsertPositionHorizontally 1 editor
    | input == cursorLeft = moveStartPositionToShowLastCursor $ moveInsertPositionHorizontally (-1) editor
    | input == cursorDown =  moveStartPosition 1 editor
    | input == cursorUp =  moveStartPosition (-1) editor
    | input == [delete] = moveStartPositionToShowLastCursor $ deleteFromEditor editor
    | otherwise = moveStartPositionToShowLastCursor $ insertIntoEditor input editor
    where
        input = bytesToString byteList

-- TODO run this on input thread so we can always stop
stopEvent :: [Int] -> Bool
stopEvent byteList
    | [escape] == input = True
    | otherwise = False
    where
        input = bytesToString byteList

-- TODO
-- ascii
-- (show input)

-- TODO
-- bytes
-- (show byteList)