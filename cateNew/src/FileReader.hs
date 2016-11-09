module FileReader (customGetContents) where

import System.IO (Handle, hSeek, SeekMode (AbsoluteSeek), hIsEOF, hGetChar)
import System.IO.Unsafe (unsafeInterleaveIO)


customGetContents :: Handle -> Integer -> IO String
customGetContents handle seekPosition = do
    hSeek handle AbsoluteSeek seekPosition
    customGetContentsAcc handle


customGetContentsAcc :: Handle -> IO String
customGetContentsAcc handle = do
    isEOF <- hIsEOF handle
    if isEOF then
        return []
    else
        do
            nextChar <- hGetChar handle
            -- we need unsafeInterleaveIO otherwise whole file is read!
            theDeferredRest <- unsafeInterleaveIO $ customGetContentsAcc handle
            return (nextChar : theDeferredRest)
