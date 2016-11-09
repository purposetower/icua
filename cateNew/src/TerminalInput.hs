module TerminalInput (loopInputProcessor) where

import Foreign (allocaArray, peekArray)
import System.Posix.IO (fdReadBuf, stdInput)


defaultInputBufferByteSize :: Int
defaultInputBufferByteSize = 1024


loopInputProcessor :: Integer -> [Int] -> (Integer -> [Int] -> IO (Bool, Integer)) -> IO ()
loopInputProcessor handlePosition byteArray inputProcessor = do
    (stopProcessing, newHandlePosition) <- inputProcessor handlePosition byteArray
    if stopProcessing then
            return ()
    else
        do
        -- allocate memory for the max number of bytes to read
        allocaArray defaultInputBufferByteSize $ \bufferPtr -> do
            -- BLOCK until we have input
            numberOfBytesRead <- fdReadBuf stdInput bufferPtr $ fromIntegral defaultInputBufferByteSize
            -- read from memory
            input <- peekArray (fromIntegral numberOfBytesRead) bufferPtr
            let byteArray = read (show input) :: [Int]
            loopInputProcessor newHandlePosition byteArray inputProcessor
