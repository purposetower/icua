module Main where

import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode))

import TerminalSize (getDisplaySize)
import DisplayText (getDisplayTextWithPadding)
import FileReader (customGetContents)
import TerminalInput (loopInputProcessor)
import TerminalRawMode (putTerminalInRawMode, resetTerminalFromRawMode)
import TerminalANSICodes

import System.IO (Handle, hFlush, stdout)
import Data.Char (chr)
import Control.Exception (finally)


main :: IO ()
main = do
    args <- getArgs

    if length args /= 1 then
        putStrLn "Pass in 1 argument which is a file path"
    else
        do
            originalTerminalAttributes <- putTerminalInRawMode

            let filePath = head args
            handle <- openFile filePath ReadMode

            let initialHandlePosition = 0
            let initialInputByteArray = []
            let inputProcessor = display handle

            -- the main loop!
            -- make sure we always restore terminal from raw mode
            finally (loopInputProcessor initialHandlePosition initialInputByteArray inputProcessor)
                (resetTerminalFromRawMode originalTerminalAttributes)


display :: Handle -> Integer -> [Int] -> IO (Bool, Integer)
display handle handlePosition inputByteArray = do
    (stopProcessing, newHandlePosition) <- handleInput handle handlePosition inputByteArray
    contents <- customGetContents handle newHandlePosition
    displaySize <- getDisplaySize
    putStr $ hideCursor ++ setCursorPositionCode (0, 0)
    putStr $ getDisplayTextWithPadding contents displaySize
    putStr showCursor
    hFlush stdout
    return (stopProcessing, newHandlePosition)


handleInput :: Handle -> Integer -> [Int] -> IO (Bool, Integer)
handleInput handle handlePosition byteArray
    -- check if user wants to exit
    | inputString == ['\ESC'] = return (True, handlePosition)

    -- left
    | inputString == cursorLeft = return (False, handlePosition - 1)
    -- right
    | inputString == cursorRight = return (False, handlePosition + 1)

    -- up
    | inputString == cursorUp = return (False, readPageUp handle (handlePosition - 3))

    -- down
    | inputString == cursorDown = return (False, handlePosition + 1)

    | inputString == "a" = undefined

    | otherwise = return (False, handlePosition)
    where
        inputString = map (chr) byteArray


readPageUp :: Handle -> Integer -> Integer
readPageUp handle handlePosition = handlePosition
    where
        contents = customGetContents handle handlePosition

-- find distance between two handle positions
