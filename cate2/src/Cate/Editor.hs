module Cate.Editor (editorRun) where

import Cate.Types.EditorTypes
import Cate.Event

import Cate.ANSICodes
import Cate.ASCIICodes
import Cate.TerminalSize
import Cate.TerminalCharacterWidth

import System.IO (openFile, IOMode (ReadMode), hSeek, SeekMode (RelativeSeek), hIsEOF, hGetLine,hGetChar, hGetPosn, hSetPosn, Handle, hPutStr, IOMode (ReadWriteMode))

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Foreign (Ptr, allocaArray, peekArray)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Posix.IO (fdReadBuf, stdInput, stdOutput)
import System.Posix.Terminal (TerminalAttributes, TerminalMode (EnableEcho, ExtendedFunctions,
    KeyboardInterrupts, ProcessInput, StartStopOutput), TerminalState (Immediately),
    getTerminalAttributes, setTerminalAttributes, withoutMode)

editorRun :: IO ()
editorRun = do
    originalTerminalAttributes <- start
    args <- getArgs
    editor <- createEditor args
    loop editor
    shutDown originalTerminalAttributes

-- save terminal attributes and put in raw mode
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

createEditor :: [String] -> IO Editor
createEditor args = do
    terminalWindowSize <- getTerminalWindowSize
    originalFileHandle <- openFile (head args) ReadWriteMode
    return (Editor defaultInputPasteByteSize defaultMaxBufferSize terminalWindowSize originalFileHandle [])

-- TODO add event system
loop :: Editor -> IO ()
loop editor = do
    displayEditor editor
    -- allocate memory
    allocaArray (inputPasteByteSize editor) $ \bufferPtr -> do
        -- BLOCK till we read bytes into memory
        bytesRead <- fdReadBuf stdInput bufferPtr $ fromIntegral (inputPasteByteSize editor)
        -- read from memory
        input <- peekArray (fromIntegral bytesRead) bufferPtr
        let byteArray = read (show input) :: [Int]
        if stopEvent byteArray then
            return ()
        else
            do
                let newEditor = processEvent byteArray editor
                loop newEditor

displayEditor :: Editor -> IO ()
displayEditor (Editor _ maxBufferSize (TerminalWindowSize width height) originalFileHandle _) = do

    -- start position before display
    pos <- hGetPosn originalFileHandle

    displaySections <- calculateDisplaySections originalFileHandle maxBufferSize width height 0 []
    putStr $ hideCursor ++ clearScreenCode ++ setCursorPositionCode (0, 0) ++
        showDisplaySections displaySections
    --putStrLn (text (head displaySections))
    --putStrLn $ foldl1 (++) $ map show displaySections

    -- turn a character into Text than encodes into a utf8 ByteString
    -- this gives us the proper byte length used for seeking
    --let byteStringUtf8EncodedString = encodeUtf8 $ T.singleton nextChar
    --B.putStrLn byteStringUtf8EncodedString
    --putStrLn $ show $ B.length byteStringUtf8EncodedString

    --pos <- hGetPosn originalFileHandle
    --putStrLn $ show pos
    --hSeek originalFileHandle RelativeSeek (-3)
    -- print immediatly
    hFlush stdout

    hSetPosn pos
    -- test move
    hSeek originalFileHandle RelativeSeek 1

showDisplaySections :: [DisplaySection] -> String
showDisplaySections [] = []
showDisplaySections (x:xs) = (text x) ++ showDisplaySections xs

calculateDisplaySections :: Handle -> Int -> Int -> Int -> Int -> [DisplaySection] -> IO [DisplaySection]
calculateDisplaySections handle maxBufferSize maxWidth height displaySectionStart displaySectionAcc = do
    -- check for end of file
    isEOF <- hIsEOF handle
    if height == 0 || isEOF then
        return displaySectionAcc
    else do
        displaySectionEnd <- calculateDisplaySectionEnd handle maxWidth (displaySectionStart, "")
        let newDisplaySection = (DisplaySection displaySectionStart (fst displaySectionEnd) (snd displaySectionEnd))
         -- go back one character
        hSeek handle RelativeSeek (-1)
        nextDisplaySectionStart <- calculateDisplaySectionStartNoWrap handle maxBufferSize (fst displaySectionEnd)
        if nextDisplaySectionStart == (-1) then do
            hPutStr handle "STOPPED HERE3"
            hFlush handle
            return (displaySectionAcc ++ [newDisplaySection])
        else do
            calculateDisplaySections handle maxBufferSize maxWidth (height - 1) nextDisplaySectionStart
                (displaySectionAcc ++ [newDisplaySection])

-- TODO class wrap and unwrap
calculateDisplaySectionStartWrap :: Int -> Int
calculateDisplaySectionStartWrap x = x

calculateDisplaySectionStartNoWrap :: Handle -> Int -> Int -> IO Int
calculateDisplaySectionStartNoWrap _ 0 displaySectionEnd = do
    -- bleugh
    return (-1)

calculateDisplaySectionStartNoWrap handle maxBufferSize displaySectionEnd = do
    -- check for end of file
    isEOF <- hIsEOF handle
    if isEOF then
        return displaySectionEnd
    else do
        nextChar <- hGetChar handle
        if nextChar == newLine then
            return (displaySectionEnd + 1)
        else
            calculateDisplaySectionStartNoWrap handle (maxBufferSize - 1) (displaySectionEnd + 1)

calculateDisplaySectionEnd :: Handle -> Int -> (Int, String) -> IO (Int, String)
calculateDisplaySectionEnd handle widthLeft acc = do
    -- check for end of file
    isEOF <- hIsEOF handle
    if isEOF then
        return acc
    else do
        nextChar <- hGetChar handle
        let characterWidth = getTerminalCharacterWidth nextChar
        let nextAcc = (fst acc + characterWidth, snd acc ++ [nextChar])

        if nextChar == newLine then
            return nextAcc
        else
            if characterWidth > widthLeft then
                return acc
            else
                calculateDisplaySectionEnd handle (widthLeft - characterWidth) nextAcc

-- don't forget to reset terminal
shutDown :: TerminalAttributes -> IO ()
shutDown originalTerminalAttributes = do
    -- add a new line
    putStrLn ""
    -- print immediatly
    hFlush stdout
    setTerminalAttributes stdInput originalTerminalAttributes Immediately
    return ()