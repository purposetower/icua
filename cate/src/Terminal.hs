module Terminal (runTerminal) where

import ANSICodes
import TerminalType
import Event
import DisplayText
import TerminalCharacterWidth
import TerminalSize
import TerminalCharacterByteSize

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
                let newTerminal = processEvent byteArray displayRows terminal
                loop newTerminal


displayTerminal :: Terminal -> IO [DisplayRow]
displayTerminal (Terminal _ handle handlePosition leftMarginDisplayOffset) = do
    -- move handler back to correct position
    hSeek handle AbsoluteSeek handlePosition
    terminalWindowSize <- getTerminalWindowSize
    fileReadIn <- makeStringy $ readSomeFileLazy handle 2000000
    let displayRows = take (height terminalWindowSize) (getDisplayRows (TextToDisplay fileReadIn handlePosition (toInteger (width terminalWindowSize)) getTerminalCharacterWidth getCharacterByteSize Wrap))
    -- remove last \n in display row
    putStr $ hideCursor ++ clearScreenCode ++ setCursorPositionCode (0, 0) ++
        (foldl1 (++) (map getTexty2 displayRows)) ++ showCursor
    hFlush stdout
    return displayRows


readSomeFileLazy :: Handle -> Integer -> [(IO Bool, IO Char)]
readSomeFileLazy _ 0 = []

readSomeFileLazy handle maxReadSize = do
    let isEOF = hIsEOF handle
    let nextChar = hGetChar handle
    let restOfFile = readSomeFileLazy handle (maxReadSize - 1)
    (isEOF, nextChar) : restOfFile

makeStringy :: [(IO Bool, IO Char)] -> IO String
makeStringy [] = return ""

makeStringy (x:xs) = do
    isEOF <- fst x
    if isEOF then
        return ""
    else do
        nextChar <- snd x
        theDeferredRest <- (unsafeInterleaveIO (makeStringy xs))
        return (nextChar : theDeferredRest)

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