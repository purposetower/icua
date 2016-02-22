module Cate.EditorLoop where

import Cate.Editor
import Cate.ProcessInput
import Cate.TerminalSize

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Posix.IO (fdReadBuf, stdInput, stdOutput)
import System.Posix.Terminal (TerminalAttributes, getTerminalAttributes, setTerminalAttributes,
    withoutMode, TerminalMode(ProcessInput, EnableEcho, KeyboardInterrupts, StartStopOutput,
    ExtendedFunctions), TerminalState(Immediately))
import Foreign (allocaArray, peekArray, Ptr)

editorRun :: IO ()
editorRun = do
    originalTerminalAttributes <- start
    args <- getArgs
    editor <- createEditor args
    loop editor
    shutDown originalTerminalAttributes

start :: IO TerminalAttributes
start = do
    originalTerminalAttributes <- getTerminalAttributes stdInput
    originalOutputTerminalAttributes <- getTerminalAttributes stdOutput

    -- put the terminal in "raw mode"(let us handle escape sequences)
    let rawTerminalAttributes = foldl withoutMode originalTerminalAttributes
            [ProcessInput, EnableEcho, KeyboardInterrupts, StartStopOutput
            , ExtendedFunctions]
    setTerminalAttributes stdInput rawTerminalAttributes Immediately
    
    return originalTerminalAttributes

loop :: Editor -> IO ()
loop editor = do
    displayEditor editor
    -- allocate memory
    allocaArray (inputBufferByteSize editor) $ \bufferPtr -> do
        -- BLOCK till we read bytes into memory
        bytesRead <- fdReadBuf stdInput bufferPtr $ fromIntegral (inputBufferByteSize editor)
        -- read from memory
        input <- peekArray (fromIntegral bytesRead) bufferPtr
        let byteArray = read (show input) :: [Int]
        if stopProcessingInput byteArray then
            return ()
        else
            do
                let newEditor = processInput byteArray editor
                -- window might have been resized
                newTerminalWindowSize <- getTerminalWindowSize
                loop newEditor {terminalWindowSize = newTerminalWindowSize}

-- don't forget to reset terminal
shutDown :: TerminalAttributes -> IO ()
shutDown originalTerminalAttributes = do
    -- add a new line
    putStrLn ""
    -- print immediatly
    hFlush stdout
    setTerminalAttributes stdInput originalTerminalAttributes Immediately
    return ()