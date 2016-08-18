module Cate (editorRun) where

import System.Environment (getArgs)
import System.IO (hFlush, openFile, IOMode (ReadMode), hSeek, SeekMode (AbsoluteSeek, RelativeSeek), hIsEOF, hGetLine,hGetChar, hGetPosn, hSetPosn, Handle, hPutStr, IOMode (ReadWriteMode))

import Data.Maybe
import Control.Monad
import System.IO.Unsafe

import FrontEnds.TerminalSize
import FrontEnds.TerminalCharacterWidth

import Core.Display
import Core.Types.CoreTypes

editorRun = do
    putStrLn "Starting..."
    args <- getArgs
    terminalWindowSize <- getTerminalWindowSize
    handle <- openFile (head args) ReadWriteMode
    let ll = readSomeFileLazy handle 10000000
    --cc <- snd $ head $ ll
    --putChar cc

    lala <- makeStringy ll
    --putStrLn $ take 5 lala

    let acc = getDisplaySectionsWrap lala 0 (width terminalWindowSize) getTerminalCharacterWidth
    let acc2 = getDisplaySectionsNoWrap lala 0 0 (width terminalWindowSize) getTerminalCharacterWidth

    putStrLn $ (foldl1 (++) (take 2000 (map text acc)))


    return ()


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