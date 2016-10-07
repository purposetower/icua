{-# LANGUAGE BangPatterns #-}

module Event (processEvent, stopEvent) where

import TerminalType
import TerminalSize
import TerminalCharWidth
import qualified LazyFileRead as LFR
import ANSICodes
import ASCIICodes
import DisplayText
import UTF8CharByteSize
import UTF8SafeByteJump

import Data.Char (chr)


-- convert bytes to String
bytesToString = map chr

processEvent :: [Int] -> [DisplayRow] -> Terminal -> IO Terminal
processEvent byteList displayRows terminal@(Terminal inputBufferByteSize handle oldHandlePosition leftMarginDisplayOffset)

    | input == cursorRight =
        -- TODO dont move handle postion when in no wrap mode, use left margin offset
        -- nowrap mode
        let nextChar = head $ getTexty $ head displayRows in
        let newHandlePosition = if nextChar == '\n' then oldHandlePosition else oldHandlePosition + (getUTF8CharByteSize nextChar) in
        return (Terminal inputBufferByteSize handle newHandlePosition (leftMarginDisplayOffset + 1))
        -- wrap mode
        --return (Terminal inputBufferByteSize handle (oldHandlePosition + (getUTF8CharByteSize nextChar)) (leftMarginDisplayOffset + 1))
    
    | input == cursorLeft = do
        newHandlePosition <- safeByteJump handle (oldHandlePosition - 1) Backward
        let newLeftMarginDisplayOffset = if leftMarginDisplayOffset <= 0 then 0 else leftMarginDisplayOffset - 1
        return (Terminal inputBufferByteSize handle newHandlePosition newLeftMarginDisplayOffset)
    
    | input == cursorDown = return (terminal {handlePosition = getStart $ last displayRows})

    | input == cursorUp = do
        newHandlePosition <- readPageUp terminal
        return (terminal {handlePosition = newHandlePosition})
    
    | otherwise = return terminal
    where
        input = bytesToString byteList
        




readPageUp (Terminal _ handle handlePosition _) = do
    terminalWindowSize <- getTerminalWindowSize
    -- read 32k up
    safeHandlePosition <- safeByteJump handle (handlePosition - 32768) Forward
    fileReadIn <- LFR.readFileLazy (LFR.LazyFileReadData handle safeHandlePosition 32768 getUTF8CharByteSize)
    let displayRows = (getDisplayRows (TextToDisplay fileReadIn safeHandlePosition (toInteger (width terminalWindowSize)) getTerminalCharWidth getUTF8CharByteSize Wrap))
    
    -- HA must force evaluation otherwise strange things happen!
    let !whichRow = whichDisplayRowHandlePosition displayRows handlePosition 0

    return (getStart $ head (drop (whichRow - (height terminalWindowSize)) displayRows))


whichDisplayRowHandlePosition [] _ acc = acc

whichDisplayRowHandlePosition (x:xs) handlePosition acc = 
    if getStart x == handlePosition then
        acc 
    else
        whichDisplayRowHandlePosition xs handlePosition (acc + 1)

getStart :: DisplayRow -> Integer
getStart (DisplayRow start _ _) = start


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