module Main where

import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode))
import Control.Exception (finally)
import System.IO (Handle, hFlush, stdout)
import Foreign (allocaArray, peekArray)
import System.Posix.IO (fdReadBuf, stdInput)
import Data.Char (chr)

import Core.LayoutText
import UI.Terminal.TerminalSize
import Plugins.Key.KeyEvent
import Plugins.Display.PadText
import UI.Terminal.TerminalANSICodes
import UI.Terminal.TerminalRawMode
import Core.FileReader

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    handle <- openFile filePath ReadMode
    originalTerminalAttributes <- putTerminalInRawMode

    -- the main loop!
    -- make sure we always restore terminal from raw mode
    finally (loop handle 0 0 []) (resetTerminalFromRawMode originalTerminalAttributes)


defaultInputBufferByteSize :: Int
defaultInputBufferByteSize = 1024

loop :: Handle -> Integer -> Integer -> PieceTable -> IO ()
loop handle handlePosition leftMargin pieceTable = do
    displaySize <- getDisplaySize
    contents <- fmap getReadContent $ getFileContent handle handlePosition pieceTable (MaxRead 200000)

    putStr $ hideCursor ++ setCursorPositionCode (0, 0)
    putStr $ foldl1 (\x y -> x ++ "\n" ++ y) $ linesPad (getLinesNoWrap contents leftMargin displaySize) displaySize
    putStr showCursor
    hFlush stdout

    -- allocate memory for the max number of bytes to read
    allocaArray defaultInputBufferByteSize $ \bufferPtr -> do
        -- BLOCK until we have input
        numberOfBytesRead <- fdReadBuf stdInput bufferPtr $ fromIntegral defaultInputBufferByteSize
        -- read from memory
        input <- peekArray (fromIntegral numberOfBytesRead) bufferPtr
        let byteArray = read (show input) :: [Int]
        let inputString = map (chr) byteArray

        if stopProcessing inputString then
            return ()
        else
            do
                (newHandlePosition, newLeftMargin, newPieceTable)
                    <- processInputNoWrap inputString displaySize handle handlePosition leftMargin pieceTable
                loop handle newHandlePosition newLeftMargin newPieceTable
