module LazyFileRead (readFileLazy, LazyFileReadData(..)) where

import System.IO (Handle, hSeek, SeekMode (AbsoluteSeek, RelativeSeek), hIsEOF, hGetChar)
import System.IO.Unsafe (unsafeInterleaveIO)

import Debug.Trace

data LazyFileReadData = LazyFileReadData {
    handle :: Handle,
    handlePosition :: Integer,
    maxReadSize :: Integer, -- max read size in bytes
    getCharBytesSize :: (Char -> Integer) -- gets the number of bytes character uses
}

readFileLazy :: LazyFileReadData -> IO String

readFileLazy (LazyFileReadData handle handlePosition maxReadSize getCharBytesSize) =
    -- move handle back to correct position
    -- we dont evaluate Io otherwise it will read whole file!
    unsafeInterleaveIO (hSeek handle AbsoluteSeek handlePosition >>
        makeStringy (readFileLazyAcc handle) maxReadSize getCharBytesSize)


readFileLazyAcc :: Handle -> [(IO Bool, IO Char)]

readFileLazyAcc handle = do
    let isEOF = hIsEOF handle
    let nextChar = hGetChar handle
    let restOfFile = readFileLazyAcc handle
    (isEOF, nextChar) : restOfFile


makeStringy :: [(IO Bool, IO Char)]
            -> Integer -- max read size in bytes
            -> (Char -> Integer) -- gets the number of bytes character uses
            -> IO String

makeStringy [] _ _ = return ""

makeStringy (x:xs) maxReadSize getCharBytesSize = do
    isEOF <- fst x
    if isEOF || maxReadSize <= 0 then
        return ""
    else do
        nextChar <- snd x
        let newMaxReadSize = maxReadSize - (getCharBytesSize nextChar)
        theDeferredRest <- makeStringy xs newMaxReadSize getCharBytesSize
        return (nextChar : theDeferredRest)
