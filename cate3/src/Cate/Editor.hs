module Cate.Editor where

import Cate.ASCIICodes
import Cate.Types.EditorTypes
import Cate.TerminalSize
import Cate.TerminalCharacterWidth

import System.IO (hFlush, openFile, IOMode (ReadMode), hSeek, SeekMode (AbsoluteSeek, RelativeSeek), hIsEOF, hGetLine,hGetChar, hGetPosn, hSetPosn, Handle, hPutStr, IOMode (ReadWriteMode))

createEditor :: [String] -> IO Editor
createEditor args = do
    terminalWindowSize <- getTerminalWindowSize
    fileHandle <- openFile (head args) ReadWriteMode
    return (Editor defaultInputPasteByteSize defaultMaxBufferSize terminalWindowSize fileHandle 0 [])

getDisplaySections :: Editor -> IO [DisplaySection]
getDisplaySections (Editor _ maxBufferSize (TerminalWindowSize width height) fileHandle displayStartPosition _) = do
    -- move handler to correct position
    hSeek fileHandle AbsoluteSeek displayStartPosition
    -- used for no wrap when moving horizontally
    let displayStartPositionOffset = (fromIntegral displayStartPosition)
    displaySections <- calculateDisplaySections fileHandle displayStartPositionOffset maxBufferSize width height (fromIntegral displayStartPosition) []    
    return displaySections

calculateDisplaySections :: Handle -> Int -> Int -> Int -> Int -> Int -> [DisplaySection] -> IO [DisplaySection]
calculateDisplaySections handle displayStartPositionOffset maxBufferSize maxWidth height displaySectionStart displaySectionAcc = do
    -- check for end of file
    isEOF <- hIsEOF handle
    if height == 0 || isEOF then
        return displaySectionAcc
    else do
        displaySectionEnd <- calculateDisplaySectionEnd handle maxWidth (displaySectionStart, "")
        let newDisplaySection = (DisplaySection displaySectionStart (fst displaySectionEnd) (snd displaySectionEnd))
        -- go back one character
        hSeek handle RelativeSeek (-1)
        nextDisplaySectionStart <- calculateDisplaySectionStartNoWrap handle displayStartPositionOffset maxBufferSize (fst displaySectionEnd)
        if nextDisplaySectionStart == (-1) then do
            hPutStr handle "STOPPED HERE3"
            hFlush handle
            return (displaySectionAcc ++ [newDisplaySection])
        else do
            skipChar handle displayStartPositionOffset
            calculateDisplaySections handle displayStartPositionOffset maxBufferSize maxWidth (height - 1) (nextDisplaySectionStart+displayStartPositionOffset)
                (displaySectionAcc ++ [newDisplaySection])


-- bleeee
skipChar handle 0 = return ()

skipChar handle n = do
    isEOF <- hIsEOF handle
    if isEOF then
        return ()
    else do
        hGetChar handle
        skipChar handle (n-1)

-- TODO class wrap and unwrap
calculateDisplaySectionStartWrap :: Int -> Int
calculateDisplaySectionStartWrap x = x

calculateDisplaySectionStartNoWrap :: Handle -> Int-> Int -> Int -> IO Int
calculateDisplaySectionStartNoWrap _ _ 0 _ = do
    -- bleugh
    return (-1)

calculateDisplaySectionStartNoWrap handle displayStartPositionOffset maxBufferSize displaySectionEnd = do
    -- check for end of file
    isEOF <- hIsEOF handle
    if isEOF then
        return displaySectionEnd
    else do
        nextChar <- hGetChar handle


        if nextChar == newLine then
            return (displaySectionEnd + 1)
        else
            calculateDisplaySectionStartNoWrap handle 0 (maxBufferSize - 1) (displaySectionEnd + 1)

calculateDisplaySectionEnd :: Handle -> Int -> (Int, String) -> IO (Int, String)
calculateDisplaySectionEnd handle widthLeft acc = do
    -- check for end of file
    isEOF <- hIsEOF handle
    if isEOF then
        return acc
    else do
        nextChar <- hGetChar handle
        let characterWidth = getTerminalCharacterWidth nextChar
        let nextAcc = (fst acc + characterWidth, snd acc ++ [nextChar])

        if nextChar == newLine then
            return nextAcc
        else
            if characterWidth > widthLeft then
                return acc
            else
                calculateDisplaySectionEnd handle (widthLeft - characterWidth) nextAcc
