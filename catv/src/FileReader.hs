module FileReader (getFileContent, ReadAmount(..), ReadContent(..), getReadContent) where

import System.IO (Handle, hSeek, SeekMode (AbsoluteSeek), hIsEOF, hGetChar)
import System.IO.Unsafe (unsafeInterleaveIO)


data ReadAmount = FullRead | MaxRead { maxReadAmount :: Integer }

data ReadContent = FullReadContent { content :: String } | MaxReadContent { content :: String }


getFileContent :: Handle -> Integer -> ReadAmount -> IO ReadContent
getFileContent handle handlePosition readAmount = do
    hSeek handle AbsoluteSeek handlePosition
    (content, fullRead) <- getFileContentAcc handle readAmount
    if fullRead
        then return (FullReadContent content)
        else return (MaxReadContent content)


getFileContentAcc :: Handle -> ReadAmount -> IO (String, Bool)
getFileContentAcc handle readAmount = do
    case readAmount of
        MaxRead 0 -> return ([], False)
        _ -> do
                isEOF <- hIsEOF handle
                if isEOF then
                    return ([], True)
                else
                    do
                        nextChar <- hGetChar handle
                        -- we need unsafeInterleaveIO otherwise whole file is read!
                        (deferredContent, deferredFullRead) <- unsafeInterleaveIO $
                            getFileContentAcc handle (case readAmount of
                                FullRead -> FullRead
                                MaxRead maxReadAmount -> MaxRead (maxReadAmount - 1))

                        return (nextChar : deferredContent, deferredFullRead)


getReadContent :: ReadContent -> String
getReadContent (FullReadContent content) = content

getReadContent (MaxReadContent content) = content