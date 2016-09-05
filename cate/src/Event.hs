module Event (processEvent, stopEvent) where

import TerminalType
import ANSICodes
import ASCIICodes
import DisplayText
import TerminalCharacterByteSize

import Data.Char (chr)

-- convert bytes to String
bytesToString = map chr

processEvent :: [Int] -> [DisplayRow] -> Terminal -> Terminal
processEvent byteList displayRows terminal@(Terminal _ _ oldHandlePosition leftMarginDisplayOffset)
    | input == cursorRight = terminal {handlePosition = oldHandlePosition + (getCharacterByteSize (head $ getTexty (head displayRows)))}
    | input == cursorLeft = terminal {handlePosition = oldHandlePosition - 1}
    | input == cursorDown = terminal
    | input == cursorUp = terminal
    | otherwise = terminal
    where
        input = bytesToString byteList

-- TODO run this on input thread so we can always stop
stopEvent :: [Int] -> Bool
stopEvent byteList
    | [escape] == input = True
    | otherwise = False
    where
        input = bytesToString byteList


getTexty (DisplayRow _ _ text) = text

-- TODO
-- ascii
-- (show input)

-- TODO
-- bytes
-- (show byteList)