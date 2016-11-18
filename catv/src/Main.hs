module Main where

import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode))
import Control.Exception (finally)
import System.IO (Handle, hFlush, stdout)
import Foreign (allocaArray, peekArray)
import System.Posix.IO (fdReadBuf, stdInput)
import Data.Char (chr)

import Prelude hiding (getContents)

import TerminalSize (getDisplaySize)
import DisplayText (getDisplayText, WrapMode (Wrap, NoWrap))
import TerminalRawMode (putTerminalInRawMode, resetTerminalFromRawMode)
import FileReader (getContents)
import TerminalANSICodes
import KeyEvent (stopProcessing, processInputWrap)


main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    handle <- openFile filePath ReadMode

    originalTerminalAttributes <- putTerminalInRawMode

    -- the main loop!
    -- make sure we always restore terminal from raw mode
    finally (loop handle 0 0) (resetTerminalFromRawMode originalTerminalAttributes)


defaultInputBufferByteSize :: Int
defaultInputBufferByteSize = 1024


loop :: Handle -> Integer -> Integer -> IO ()
loop handle handlePosition leftMarginDisplayOffset = do
    contents <- getContents handle handlePosition
    displaySize <- getDisplaySize

    putStr $ hideCursor ++ setCursorPositionCode (0, 0)
    putStr $ getDisplayText contents displaySize (Wrap) True
    putStr showCursor
    hFlush stdout

    -- allocate memory for the max number of bytes to read
    allocaArray defaultInputBufferByteSize $ \bufferPtr -> do
        -- BLOCK until we have input
        numberOfBytesRead <- fdReadBuf stdInput bufferPtr $ fromIntegral defaultInputBufferByteSize
        -- read from memory
        input <- peekArray (fromIntegral numberOfBytesRead) bufferPtr
        let byteArray = read (show input) :: [Int]
        let inputString = map (chr) byteArray

        if stopProcessing inputString then
            return ()
        else
            do
                newHandlePosition <-
                    processInputWrap inputString handle handlePosition
                    
                loop handle newHandlePosition leftMarginDisplayOffset
