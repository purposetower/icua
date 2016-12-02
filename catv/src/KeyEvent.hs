{-# LANGUAGE BangPatterns #-}

module KeyEvent (stopProcessing, processInput) where

import System.IO (Handle, hFileSize)

import LayoutText
import TerminalANSICodes
import FileReader

stopProcessing :: String -> Bool
stopProcessing input = input == ['\ESC']


processInput :: String -> DisplaySize -> Handle -> Integer -> WrapMode -> IO (Integer, Integer)
processInput input displaySize handle handlePosition wrapMode = case wrapMode of
    Wrap -> processInputWrap input displaySize handle handlePosition 0
    NoWrap leftMargin -> processInputNoWrap input displaySize handle handlePosition leftMargin


processInputWrap :: String -> DisplaySize -> Handle -> Integer -> Integer -> IO (Integer, Integer)
processInputWrap input displaySize@(DisplaySize width height) handle handlePosition leftMargin
    -- find the previous new line, if it's less than width go to that otherwise jump back width
    | input == cursorUp = do
        offset <- findPreviousLine handle handlePosition
        newHandlePosition <- if offset < width
            then handlePositionBoundary handle (handlePosition - offset)
            else handlePositionBoundary handle (handlePosition - width)
        return (newHandlePosition, leftMargin)
    
    -- find the next new line, if it's less than width go to that otherwise jump forward width
    | input == cursorDown = do
        contents <- getFileContents handle handlePosition
        -- check we haven't reached the end of the file
        let numberOfLinesLeft = toInteger $ length $ getLinesWrap contents displaySize
        if height > numberOfLinesLeft
            then
                return (handlePosition, leftMargin)
            else
                do
                    offset <- findNextLine handle handlePosition
                    newHandlePosition <- if offset < width
                        then handlePositionBoundary handle (handlePosition + offset)
                        else handlePositionBoundary handle (handlePosition + width)
                    return (newHandlePosition, leftMargin)

    | otherwise = return (handlePosition, leftMargin)


processInputNoWrap :: String -> DisplaySize -> Handle -> Integer -> Integer -> IO (Integer, Integer)
processInputNoWrap input displaySize@(DisplaySize width height) handle handlePosition leftMargin
    -- move left margin left unless 0
    | input == cursorLeft = return (handlePosition, if leftMargin > 0 then leftMargin - 1 else 0)

    -- move left margin right
    | input == cursorRight = do
        contents <- getFileContents handle handlePosition
        -- get the max line length, stop user from moving right if there is no more content to show
        -- + 2 to leftMargin as we are testing next positiong and we ignore last new line
        let !maxLineLength = toInteger $ maximum $ map length $ getLinesNoWrap contents (leftMargin + 2) displaySize
        return (handlePosition, if maxLineLength >= width then leftMargin + 1 else leftMargin)

    -- find the previous new line and go to that
    | input == cursorUp = do
        offset <- findPreviousLine handle handlePosition
        newHandlePosition <- handlePositionBoundary handle (handlePosition - offset)
        return (newHandlePosition, leftMargin)

    -- find the next new line and go to that
    | input == cursorDown = do
        contents <- getFileContents handle handlePosition
        -- check we haven't reached the end of the file
        let numberOfLinesLeft = toInteger $ length $ getLinesNoWrap contents leftMargin displaySize
        if height > numberOfLinesLeft
            then
                return (handlePosition, leftMargin)
            else
                do
                    offset <- findNextLine handle handlePosition
                    newHandlePosition <- handlePositionBoundary handle (handlePosition + offset)
                    return (newHandlePosition, leftMargin)

    | otherwise = return (handlePosition, leftMargin)


defaultLookBack :: Integer
defaultLookBack = 150

findPreviousLine :: Handle -> Integer -> IO Integer
findPreviousLine handle handlePosition = do
    let lookBack = if handlePosition - defaultLookBack > 0 then defaultLookBack else handlePosition
    contents <- getFileContents handle (handlePosition - lookBack)
    let reversedContent = reverse $ take (fromIntegral (lookBack - 1)) contents
    let !offset = nextNewLineOffset reversedContent
    return offset


findNextLine :: Handle -> Integer -> IO Integer
findNextLine handle handlePosition = do
    contents <- getFileContents handle handlePosition
    let !offset = nextNewLineOffset contents
    return offset


nextNewLineOffset :: String -> Integer
-- no new line!
nextNewLineOffset "" = 0

nextNewLineOffset input = (toInteger $ length $ takeWhile (/= '\n') input) + 1


handlePositionBoundary :: Handle -> Integer -> IO Integer
handlePositionBoundary handle handlePosition 
    | handlePosition <= 0 = return 0
    | otherwise = do
        fileSize <- hFileSize handle
        return $ if handlePosition > fileSize then fileSize else handlePosition
