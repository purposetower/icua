module Cate.ProcessInput where

import Cate.ASCIICodes
import Cate.ANSICodes
import Cate.Buffer

import Data.Char (chr)

-- convert bytes to String
bytesToString = map chr

-- how to display output
data ShowMode = Normal | AsciiChar | Bytes

processInput :: ShowMode -> [Int] -> Buffer -> Buffer
processInput Normal byteList buffer@(Buffer cPosition bData tWSize)
    | input == formFeed = Buffer cPosition bData tWSize
    
    | input == cursorUp = Buffer cPosition bData tWSize
    | input == cursorDown = Buffer cPosition bData tWSize
    | input == cursorRight = Buffer cPosition bData tWSize
    | input == cursorLeft = Buffer cPosition bData tWSize
    
    | input == delete = Buffer (cPosition - 1)
        (bufferData (removeFromBuffer buffer (cPosition - 1) cPosition)) tWSize
    
    | input == newLine = insertNewLineIntoBuffer buffer
    
    | otherwise = insertIntoBuffer buffer input
    where input = bytesToString byteList

processInput AsciiChar byteList buffer@(Buffer cPosition bData tWSize)
    | otherwise = insertIntoBuffer buffer $ show input
    where input = bytesToString byteList

processInput Bytes byteList buffer@(Buffer cPosition bData tWSize)
    | otherwise = insertIntoBuffer buffer $ show byteList
    where input = bytesToString byteList

stopProcessingInput :: [Int] -> Bool
stopProcessingInput byteList
    | input == escape = True
    | otherwise = False
    where input = bytesToString byteList
