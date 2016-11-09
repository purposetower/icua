import Cate.ANSICodes
import Cate.ASCIICodes
import Cate.Types.EditorTypes
import Cate.Editor
import Cate.Display

import Foreign (Ptr, allocaArray, peekArray)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Posix.IO (fdReadBuf, stdInput, stdOutput)
import Data.Char (chr)

main = editorRun

editorRun :: IO ()
editorRun = do
    originalTerminalAttributes <- start
    args <- getArgs
    editor <- createEditor args
    loop editor
    shutdown originalTerminalAttributes

loop :: Editor -> IO ()
loop editor = do
    displaySections <- getDisplaySections editor
    display displaySections
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


-- convert bytes to String
bytesToString = map chr

processEvent :: [Int] -> Editor -> Editor
processEvent byteList editor@(Editor _ _ _ _ oldDisplayStartPosition _)
    | input == cursorRight = editor {displayStartPosition =  oldDisplayStartPosition + 1}
    | input == cursorLeft = editor {displayStartPosition =  oldDisplayStartPosition - 1}
    | input == cursorDown =  editor {displayStartPosition =  oldDisplayStartPosition + 10}
    | input == cursorUp =  editor {displayStartPosition =  oldDisplayStartPosition - 10}
    | input == [delete] = editor
    | otherwise = editor
    where
        input = bytesToString byteList

-- TODO run this on input thread so we can always stop
stopEvent :: [Int] -> Bool
stopEvent byteList
    | [escape] == input = True
    | otherwise = False
    where
        input = bytesToString byteList