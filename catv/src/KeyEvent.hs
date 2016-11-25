{-# LANGUAGE BangPatterns #-}

module KeyEvent (stopProcessing, processInput) where

import System.IO (Handle)

import Prelude hiding (getContents)

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
processInputWrap input displaySize handle handlePosition leftMargin
    | input == cursorUp = return (handlePosition, leftMargin)
    | input == cursorDown = return (handlePosition, leftMargin)

    | otherwise = return (handlePosition, leftMargin)


processInputNoWrap :: String -> DisplaySize -> Handle -> Integer -> Integer -> IO (Integer, Integer)
processInputNoWrap input displaySize@(DisplaySize width height) handle handlePosition leftMargin
    | input == cursorLeft = return (handlePosition, if leftMargin > 0 then leftMargin - 1 else 0)

    | input == cursorRight = do
        contents <- getContents handle handlePosition
        -- + 2 to leftMargin as we are testing next positiong and we ignore last new line
        let !maxLineLength = toInteger $ maximum $ map length $ getLinesNoWrap contents (leftMargin + 2) displaySize
        return (handlePosition, if maxLineLength >= width then leftMargin + 1 else leftMargin)

    | input == cursorUp = do
        newHandlePosition <- findPreviousLine handle handlePosition
        return (newHandlePosition, leftMargin)

    | input == cursorDown = do
        contents <- getContents handle handlePosition
        let !nextLine = toInteger $ length (takeWhile (/= '\n') contents) + 1
        return (handlePosition + nextLine, leftMargin)

    | otherwise = return (handlePosition, leftMargin)



defaultLookBack :: Integer
defaultLookBack = 150

findPreviousLine :: Handle -> Integer -> IO Integer
findPreviousLine handle handlePosition = do
    let lookBack = if handlePosition - defaultLookBack > 0 then defaultLookBack else handlePosition
    contents <- getContents handle (handlePosition - lookBack)
    let !result = findPreviousLineNoWrap $ reverse $ take ((fromIntegral lookBack) - 1) $ contents
    return $ handlePosition - result

findPreviousLineNoWrap :: String -> Integer
findPreviousLineNoWrap "" = 0

findPreviousLineNoWrap input = (toInteger $ length $ takeWhile (/= '\n') $ init input) + 1