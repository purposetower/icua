{-# LANGUAGE BangPatterns #-}

module KeyEvent (stopProcessing, processInput, processInputWrap) where

import System.IO (Handle)

import Prelude hiding (getContents)

import FileReader (getContents)
import TerminalANSICodes

stopProcessing :: String -> Bool
stopProcessing input = input == ['\ESC']


processInput :: String -> Handle -> Integer -> Integer -> IO (Integer, Integer)
processInput input handle handlePosition leftMarginDisplayOffset

    | input == cursorLeft =  return $ (handlePosition, leftMarginDisplayOffset - 1)
    | input == cursorRight = return $ (handlePosition, leftMarginDisplayOffset + 1)

    | input == cursorUp = do
        newHandlePosition <- findPreviousLine handle handlePosition
        return (newHandlePosition, leftMarginDisplayOffset)

    | input == cursorDown = do
        newHandlePosition <- findNextLine handle handlePosition
        return (newHandlePosition, leftMarginDisplayOffset)

    | otherwise = return (handlePosition, leftMarginDisplayOffset)


processInputWrap :: String -> Handle -> Integer -> IO Integer
processInputWrap input handle handlePosition

    | input == cursorUp = do
        newHandlePosition <- findPreviousLine handle handlePosition
        return newHandlePosition

    | input == cursorDown = do
        newHandlePosition <- findNextLine handle handlePosition
        return newHandlePosition

    | otherwise = return handlePosition


findNextLine :: Handle -> Integer -> IO Integer
findNextLine handle handlePosition = do
    contents <- getContents handle handlePosition
    let !result = toInteger $ length (takeWhile (/= '\n') contents) + 1
    return $ result + handlePosition


-- pass width
--findNextLineWrap :: Handle -> Integer -> IO Integer
--findNextLineWrap handle handlePosition = do
--    contents <- getContents handle handlePosition


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
