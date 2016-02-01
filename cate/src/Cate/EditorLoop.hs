module Cate.EditorLoop where

import Cate.ANSICodes
import Cate.ProcessInput
import Cate.Buffer

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Posix.IO (fdReadBuf, stdInput, stdOutput)
import System.Posix.Terminal (TerminalAttributes, getTerminalAttributes, setTerminalAttributes,
    withoutMode, TerminalMode(ProcessInput, EnableEcho, KeyboardInterrupts, StartStopOutput,
    ExtendedFunctions), TerminalState(Immediately))
import Foreign (allocaArray, peekArray, Ptr)

-- how much the user can type/paste in
inputBufferByteSize = 1024

editorRun :: IO ()
editorRun = do
    args <- getArgs
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
    allocaArray inputBufferByteSize $ \bufferPtr -> do
        -- block till we read in bytes into memory
        bytesRead <- fdReadBuf stdInput bufferPtr $ fromIntegral inputBufferByteSize
        -- read from memory
        input <- peekArray (fromIntegral bytesRead) bufferPtr
        let byteArray = read (show input) :: [Int]
        if stopProcessingInput byteArray then
            return ()
        else
            do
                newBuffer <- displayBuffer $ processInput Normal byteArray buffer
                -- finally flush the output out
                hFlush stdout
                loop newBuffer

-- don't forget to reset terminal
shutDown :: TerminalAttributes -> IO ()
shutDown originalTerminalAttributes = do
    setTerminalAttributes stdInput originalTerminalAttributes Immediately
    return ()
