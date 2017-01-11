{-# LANGUAGE BangPatterns #-}

module Plugins.Key.KeyEvent (stopProcessing, processInputWrap, processInputNoWrap) where

import System.IO (Handle, hFileSize)

import Core.Types.DisplaySize
import Core.LayoutText
import UI.Terminal.TerminalANSICodes
import Core.FileReader

stopProcessing :: String -> Bool
stopProcessing input = input == ['\ESC']


processInputWrap :: String -> DisplaySize -> Handle -> Integer -> PieceTable -> IO (Integer, PieceTable)
processInputWrap input displaySize@(DisplaySize width height) handle handlePosition pieceTable
    -- find the previous new line, if it's less than width go to that otherwise jump back width
    | input == cursorUp = do
        offset <- findPreviousLine handle handlePosition pieceTable
        newHandlePosition <- if offset < width
            then handlePositionBoundary handle (handlePosition - offset)
            else handlePositionBoundary handle (handlePosition - width)
        return (newHandlePosition, pieceTable)
    
    -- find the next new line, if it's less than width go to that otherwise jump forward width
    | input == cursorDown = do
        contents <- fmap getReadContent $ getFileContent handle handlePosition pieceTable FullRead
        -- check we haven't reached the end of the file
        let numberOfLinesLeft = toInteger $ length $ getLinesWrap contents displaySize
        if height > numberOfLinesLeft
            then
                return (handlePosition, pieceTable)
            else
                do
                    offset <- findNextLine handle handlePosition pieceTable
                    newHandlePosition <- if offset < width
                        then handlePositionBoundary handle (handlePosition + offset)
                        else handlePositionBoundary handle (handlePosition + width)
                    return (newHandlePosition, pieceTable)

    | otherwise = return (handlePosition, pieceTable)


processInputNoWrap :: String -> DisplaySize -> Handle -> Integer -> Integer -> PieceTable -> IO (Integer, Integer, PieceTable)
processInputNoWrap input displaySize@(DisplaySize width height) handle handlePosition leftMargin pieceTable
    -- move left margin left unless 0
    | input == cursorLeft = return (handlePosition, if leftMargin > 0 then leftMargin - 1 else 0, pieceTable)

    -- move left margin right
    | input == cursorRight = do
        -- get the max line length, stop user from moving right if there is no more content to show
        contents <- fmap getReadContent $ getFileContent handle handlePosition pieceTable
            (MaxRead ((width + 1 + leftMargin) * height))
        -- + 1 to leftMargin as we are testing next positiong
        let !maxLineLength = toInteger $ maximum $ map length $ getLinesNoWrap contents (leftMargin + 1) displaySize
        return (handlePosition, if maxLineLength >= width then leftMargin + 1 else leftMargin, pieceTable)

    -- find the previous new line and go to that
    | input == cursorUp = do
        offset <- findPreviousLine handle handlePosition pieceTable
        newHandlePosition <- handlePositionBoundary handle (handlePosition - offset)
        return (newHandlePosition, leftMargin, pieceTable)

    -- find the next new line and go to that
    | input == cursorDown = do
        contents <- fmap getReadContent $ getFileContent handle handlePosition pieceTable FullRead
        -- check we haven't reached the end of the file
        let numberOfLinesLeft = toInteger $ length $ getLinesNoWrap contents leftMargin displaySize
        if height > numberOfLinesLeft
            then
                return (handlePosition, leftMargin, pieceTable)
            else
                do
                    offset <- findNextLine handle handlePosition pieceTable
                    newHandlePosition <- handlePositionBoundary handle (handlePosition + offset)
                    return (newHandlePosition, leftMargin, pieceTable)

    | otherwise = return (handlePosition, leftMargin, (PieceTableItem (handlePosition + leftMargin) input) : pieceTable)


defaultLookBack :: Integer
defaultLookBack = 150

findPreviousLine :: Handle -> Integer -> PieceTable -> IO Integer
findPreviousLine handle handlePosition pieceTable = do
    let lookBack = if handlePosition - defaultLookBack > 0 then defaultLookBack else handlePosition
    contents <- fmap getReadContent $ getFileContent handle (handlePosition - lookBack) pieceTable FullRead
    let reversedContent = reverse $ take (fromIntegral (lookBack - 1)) contents
    let offset = nextNewLineOffset reversedContent
    return offset


findNextLine :: Handle -> Integer -> PieceTable -> IO Integer
findNextLine handle handlePosition pieceTable = do
    contents <- fmap getReadContent $ getFileContent handle handlePosition pieceTable FullRead
    let offset = nextNewLineOffset contents
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
