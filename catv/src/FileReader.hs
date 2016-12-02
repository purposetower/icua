module FileReader (getFileContents) where

import System.IO (Handle, hSeek, SeekMode (AbsoluteSeek), hIsEOF, hGetChar)
import System.IO.Unsafe (unsafeInterleaveIO)


getFileContents :: Handle -> Integer -> IO String
getFileContents handle handlePosition = do
    hSeek handle AbsoluteSeek handlePosition
    getFileContentsAcc handle


getFileContentsAcc :: Handle -> IO String
getFileContentsAcc handle = do
    isEOF <- hIsEOF handle
    if isEOF then
        return []
    else
        do
            nextChar <- hGetChar handle
            -- we need unsafeInterleaveIO otherwise whole file is read!
            theDeferredRest <- unsafeInterleaveIO $ getFileContentsAcc handle
            return (nextChar : theDeferredRest)
