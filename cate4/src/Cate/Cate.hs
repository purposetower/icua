module Cate (editorRun) where

import System.Environment (getArgs)
import System.IO (hFlush, openFile, IOMode (ReadMode), hSeek, SeekMode (AbsoluteSeek, RelativeSeek), hIsEOF, hGetLine,hGetChar, hGetPosn, hSetPosn, Handle, hPutStr, IOMode (ReadWriteMode))

import Data.Maybe
import Control.Monad
import System.IO.Unsafe

import FrontEnds.TerminalSize
import FrontEnds.TerminalCharacterWidth

import Core.Display
import Core.Display2
import Core.Types.CoreTypes
import Core.Types.CoreTypes2

import qualified Codec.Binary.UTF8.String as U

import Debug.Trace

-- can debug by doing `debug` someString after stuff
debug = flip trace

editorRun = do
    putStrLn "Starting..."
    args <- getArgs
    terminalWindowSize <- getTerminalWindowSize
    handle <- openFile (head args) ReadWriteMode
    let ll = readSomeFileLazy handle 10000000
    --cc <- snd $ head $ ll
    --putChar cc

    lala <- makeStringy ll
    --putStrLn lala

    let newDisplay2Test = getDisplayRows (TextToDisplay lala 0 (width terminalWindowSize) getTerminalCharacterWidth getUtf8CharBytesLength (NoWrap 0 134))
    --putStrLn $ show (length newDisplay2Test)

    putStrLn $ (foldl1 (++) (take 20 (map getTexty2 newDisplay2Test)))

    --let acc = getDisplaySectionsWrap lala 0 (width terminalWindowSize) getTerminalCharacterWidth
    --let acc2 = getDisplaySectionsNoWrap lala 0 0 (width terminalWindowSize) getTerminalCharacterWidth

    --putStrLn $ (foldl1 (++) (take 2000 (map getTexty acc)))


    return ()

-- get the number of bytes a utf8 character takes
getUtf8CharBytesLength :: Char -> Integer
getUtf8CharBytesLength = toInteger . length . U.encodeChar

getTexty (DisplaySection _ _ text) = text

getTexty2 (DisplayRow _ _ text) = text

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


readSomeFileLazy :: Handle -> Integer -> [(IO Bool, IO Char)]
readSomeFileLazy _ 0 = []

readSomeFileLazy handle maxReadSize = do
    let isEOF = hIsEOF handle
    let nextChar = hGetChar handle
    let restOfFile = readSomeFileLazy handle (maxReadSize - 1)
    (isEOF, nextChar) : restOfFile