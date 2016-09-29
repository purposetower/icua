module Terminal (runTerminal) where

import ANSICodes
import TerminalType
import Event
import LazyFileRead
import DisplayText
import TerminalCharWidth
import TerminalSize
import UTF8CharByteSize

import Foreign (Ptr, allocaArray, peekArray)
import System.Environment (getArgs)
import System.IO (Handle, openFile, IOMode (ReadMode), hFlush, hIsEOF, hGetChar, stdout, hSeek, SeekMode (AbsoluteSeek))
import System.IO.Unsafe
import System.Posix.IO (fdReadBuf, stdInput, stdOutput)
import System.Posix.Terminal (TerminalAttributes, TerminalMode (EnableEcho, ExtendedFunctions,
    KeyboardInterrupts, ProcessInput, StartStopOutput), TerminalState (Immediately),
    getTerminalAttributes, setTerminalAttributes, withoutMode)

-- how many bytes the user can paste in
defaultInputBufferByteSize :: Int
defaultInputBufferByteSize = 1024

createTerminal :: [String] -> IO Terminal
createTerminal args = do
    handle <- openFile (head args) ReadMode
    return (Terminal defaultInputBufferByteSize handle 0 0)

runTerminal :: IO ()
runTerminal = do
    originalTerminalAttributes <- startTerminal
    args <- getArgs
    terminal <- createTerminal args
    loop terminal
    shutDownTerminal originalTerminalAttributes

startTerminal :: IO TerminalAttributes
startTerminal = do
    originalTerminalAttributes <- getTerminalAttributes stdInput
    originalOutputTerminalAttributes <- getTerminalAttributes stdOutput

    -- put the terminal in "raw mode"(let us handle escape sequences)
    let rawTerminalAttributes = foldl withoutMode originalTerminalAttributes
            [ProcessInput, EnableEcho, KeyboardInterrupts, StartStopOutput
            , ExtendedFunctions]
    setTerminalAttributes stdInput rawTerminalAttributes Immediately

    return originalTerminalAttributes

-- TODO don't block UI thread
loop :: Terminal -> IO ()
loop terminal = do
    displayRows <- displayTerminal terminal
    -- allocate memory
    allocaArray (inputBufferByteSize terminal) $ \bufferPtr -> do
        -- BLOCK till we read bytes into memory
        bytesRead <- fdReadBuf stdInput bufferPtr $ fromIntegral (inputBufferByteSize terminal)
        -- read from memory
        input <- peekArray (fromIntegral bytesRead) bufferPtr
        let byteArray = read (show input) :: [Int]
        if stopEvent byteArray then
            return ()
        else
            do
                newTerminal <- processEvent byteArray displayRows terminal
                loop newTerminal


displayTerminal :: Terminal -> IO [DisplayRow]
displayTerminal (Terminal _ handle handlePosition leftMarginDisplayOffset) = do
    terminalWindowSize <- getTerminalWindowSize
    fileReadIn <- readFileLazy (LazyFileReadData handle handlePosition 32768 getUTF8CharByteSize)
    let displayRows = take (height terminalWindowSize) (getDisplayRows (TextToDisplay fileReadIn handlePosition (toInteger (width terminalWindowSize)) getTerminalCharWidth getUTF8CharByteSize Wrap))
    -- remove last \n in display row
    putStr $ hideCursor ++ clearScreenCode ++ setCursorPositionCode (0, 0) ++
        printDisplayRows displayRows ++ showCursor
    hFlush stdout
    return displayRows


printDisplayRows displayRows = if (last toPrint) == '\n' then init toPrint else toPrint
    where
        toPrint = foldl (++) "" (map getTexty2 displayRows)

getTexty2 (DisplayRow _ _ text) = text

-- don't forget to reset terminal
shutDownTerminal :: TerminalAttributes -> IO ()
shutDownTerminal originalTerminalAttributes = do
    -- add a new line
    putStrLn ""
    -- print immediatly
    hFlush stdout
    setTerminalAttributes stdInput originalTerminalAttributes Immediately
    return ()