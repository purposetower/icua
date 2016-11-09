module Cate.Editor (editorRun) where

import Cate.Buffer
import Cate.Display (createDisplay, displayEditor)
import Cate.Event
import Cate.Types.EditorTypes (Editor (..), defaultInputBufferByteSize)

import Foreign (Ptr, allocaArray, peekArray)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Posix.IO (fdReadBuf, stdInput, stdOutput)
import System.Posix.Terminal (TerminalAttributes, TerminalMode (EnableEcho, ExtendedFunctions,
    KeyboardInterrupts, ProcessInput, StartStopOutput), TerminalState (Immediately),
    getTerminalAttributes, setTerminalAttributes, withoutMode)

createEditor :: [String] -> IO Editor
createEditor input = do
    buffer <- createBuffer input
    display <- createDisplay
    return (Editor defaultInputBufferByteSize buffer display)

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

-- TODO don't block UI thread
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
        if stopEvent byteArray then
            return ()
        else
            do
                let newEditor = processEvent byteArray editor
                loop newEditor

-- don't forget to reset terminal
shutDown :: TerminalAttributes -> IO ()
shutDown originalTerminalAttributes = do
    -- add a new line
    putStrLn ""
    -- print immediatly
    hFlush stdout
    setTerminalAttributes stdInput originalTerminalAttributes Immediately
    return ()
