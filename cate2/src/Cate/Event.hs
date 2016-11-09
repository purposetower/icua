module Cate.Event (processEvent, stopEvent) where

import Cate.ANSICodes
import Cate.ASCIICodes
import Cate.Types.EditorTypes (Editor (..))

import Data.Char (chr)

-- convert bytes to String
bytesToString = map chr

processEvent :: [Int] -> Editor -> Editor
processEvent byteList editor
    | input == cursorRight = editor
    | input == cursorLeft = editor
    | input == cursorDown =  editor
    | input == cursorUp =  editor
    | input == [delete] = editor
    | otherwise = editor
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