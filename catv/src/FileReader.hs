module FileReader (getContents) where

import Prelude hiding (getContents)

import System.IO (Handle, hSeek, SeekMode (AbsoluteSeek), hIsEOF, hGetChar)
import System.IO.Unsafe (unsafeInterleaveIO)


getContents :: Handle -> Integer -> IO String
getContents handle handlePosition = do
    hSeek handle AbsoluteSeek handlePosition
    getContentsAcc handle


getContentsAcc :: Handle -> IO String
getContentsAcc handle = do
    isEOF <- hIsEOF handle
    if isEOF then
        return []
    else
        do
            nextChar <- hGetChar handle
            -- we need unsafeInterleaveIO otherwise whole file is read!
            theDeferredRest <- unsafeInterleaveIO $ getContentsAcc handle
            return (nextChar : theDeferredRest)
