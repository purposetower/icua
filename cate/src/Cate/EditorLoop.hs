module Cate.EditorLoop where

import Cate.ANSICodes
import Cate.ProcessInput
import Cate.Buffer

import System.IO (hFlush, stdout)
import System.Posix.IO (fdReadBuf, stdInput, stdOutput)
import System.Posix.Terminal
import Foreign (allocaArray, peekArray, Ptr)

editorRun :: IO ()
editorRun = do
    originalTerminalAttributes <- start
    loop createEmptyBuffer
    shutDown originalTerminalAttributes

start :: IO TerminalAttributes
start = do
    -- get the original terminal settings
    originalTerminalAttributes <- getTerminalAttributes stdInput
    originalOutputTerminalAttributes <- getTerminalAttributes stdOutput
    
    -- put the terminal in "raw mode"(let us handle escape sequences)
    let rawTerminalAttributes = foldl withoutMode originalTerminalAttributes
            [ProcessInput, EnableEcho, KeyboardInterrupts, StartStopOutput
            , ExtendedFunctions]
    setTerminalAttributes stdInput rawTerminalAttributes Immediately
    
    putStr $ clearScreenCode
    putStr $ setCursorPositionCode 0 0
    -- print immediatly
    hFlush stdout
    return originalTerminalAttributes

loop :: Buffer -> IO ()
loop buffer = do
    -- allocate memory
    -- TODO 1024 bytes memory? why?
    allocaArray 1024 $ \bufferPtr -> do
        -- block till we read in bytes into memory
        bytesRead <- fdReadBuf stdInput bufferPtr 1024
        -- read from memory
        input <- peekArray (fromIntegral bytesRead) bufferPtr
        let byteArray = read (show input) :: [Int]
        if stopProcessingInput byteArray then
            return ()
        else
            do
                newBuffer <- displayBuffer $ processInput AsciiChar byteArray buffer
                -- finally flush the output out
                hFlush stdout
                loop newBuffer

-- don't forget to reset terminal
shutDown :: TerminalAttributes -> IO ()
shutDown originalTerminalAttributes = do
    setTerminalAttributes stdInput originalTerminalAttributes Immediately
    return ()
