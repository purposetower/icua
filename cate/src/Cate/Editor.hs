module Cate.Editor where

import Cate.TerminalSize
import Cate.ANSICodes
import Cate.ASCIICodes

import System.IO (hFlush, stdout, openFile, IOMode(ReadMode), hGetContents)

-- how many bytes the user can paste in
defaultInputBufferByteSize = 1024

data Editor = Editor {inputBufferByteSize :: Int, terminalWindowSize :: TerminalWindowSize,
    displayStartPosition :: Int, insertPosition :: Int, bufferData :: String}

createEditor :: [String] -> IO Editor
createEditor [] = do
    terminalWindowSize <- getTerminalWindowSize
    -- empty editor
    return (Editor defaultInputBufferByteSize terminalWindowSize 0 0 "")

createEditor (x:xs) = do
    terminalWindowSize <- getTerminalWindowSize
    -- ignore everything after first arg
    fileHandle <- openFile x ReadMode
    fileContents <- hGetContents fileHandle
    return (Editor defaultInputBufferByteSize terminalWindowSize 0 0 fileContents)

displayEditor :: Editor -> IO ()
displayEditor editor = do
        putStr $ clearScreenCode ++ setCursorPositionCode (0, 0) ++
            getViewableData editor ++ getCursorPosition editor
        -- print immediatly
        hFlush stdout

-- returns data we can view starting from displayStartPosition
getViewableData :: Editor -> String
getViewableData (Editor _ terminalWindowSize@(TerminalWindowSize width height)
    displayStartPosition _ bufferData) =
    viewableData width height terminalWindowSize (drop displayStartPosition bufferData)

getCursorPosition :: Editor -> String
getCursorPosition (Editor _ terminalWindowSize@(TerminalWindowSize width height)
    displayStartPosition insertPosition bufferData) = setCursorPositionCode $
    viewableCursor 0 0 terminalWindowSize $ drop displayStartPosition $
    take insertPosition bufferData

-- works out how much data can be displayed on terminal
-- takes newLines into account
-- takes width accumulator, height accumulator, terminal size, text to fit in terminal
viewableData :: Int -> Int -> TerminalWindowSize -> String -> String
-- no more data
viewableData _ _ _ [] = ""

-- end of terminal viewable area, 0 height
viewableData _ 0 _ _ = ""

viewableData accWidth accHeight terminalWindowSize@(TerminalWindowSize width _) (x:xs)
    -- handle new line on last line of terminal
    | x == newLine && accHeight == 1 = ""
    -- ignore new line when last character fitting on terminal width
    | x == newLine && accWidth == 1 = x : viewableData width accHeight terminalWindowSize xs
    -- handle new line
    | x == newLine = x : viewableData width (accHeight - 1) terminalWindowSize xs
    -- end of terminal width, move down
    | accWidth == 1 = x : viewableData width (accHeight - 1) terminalWindowSize  xs
    -- move right
    | otherwise = x : viewableData (accWidth - 1) accHeight terminalWindowSize  xs

-- works out cursor position given we start at displayStartPosition and end at insertPosition
-- takes width accumulator, height accumulator, terminal size, text to fit in terminal
viewableCursor :: Int -> Int -> TerminalWindowSize -> String -> (Int, Int)

viewableCursor accWidth accHeight _ [] = (accHeight, accWidth)

viewableCursor accWidth accHeight terminalWindowSize@(TerminalWindowSize width _) (x:xs)
    -- handle new line
    | x == newLine = viewableCursor 0 (accHeight + 1) terminalWindowSize xs
    -- end of terminal width, move down
    | accWidth == width - 1 = viewableCursor 0 (accHeight + 1) terminalWindowSize  xs
    -- move right
    | otherwise = viewableCursor (accWidth + 1) accHeight terminalWindowSize  xs

insertIntoEditor :: String -> Editor -> Editor
insertIntoEditor input editor@(Editor _ _ _ insertPosition bufferData) =
    editor {insertPosition = insertPosition + length input, bufferData =
        ((take insertPosition bufferData) ++ input ++ (drop insertPosition bufferData))}

deleteFromEditor :: Editor -> Editor
deleteFromEditor editor@(Editor _ _ _ insertPosition bufferData)
    | newInsertPosition >= 0 = editor {insertPosition = newInsertPosition, bufferData =
        ((take newInsertPosition bufferData) ++ (drop insertPosition bufferData))}
    | otherwise = editor
    where
        newInsertPosition = insertPosition - 1

-- moves displayStartPosition until we can see cursor
moveDisplayStartPosition :: Editor -> Editor
moveDisplayStartPosition editor@(Editor _ terminalWindowSize@(TerminalWindowSize width height)
    displayStartPosition insertPosition bufferData)
    | fromInsertPostionToDisplayStartPositionLength < insertPosition - displayStartPosition =
        editor {displayStartPosition = insertPosition - fromInsertPostionToDisplayStartPositionLength}
    | insertPosition < displayStartPosition = editor {displayStartPosition = insertPosition}
    | otherwise = editor
    where
        fromInsertPostionToDisplayStartPositionLength = length $ viewableData width height
            terminalWindowSize (reverse $ take insertPosition bufferData)

moveInsertPositionHorizontal :: Int -> Editor -> Editor
moveInsertPositionHorizontal moveAmount editor@(Editor _ _ _ insertPosition bufferData)
    | newInsertPosition >= 0 && newInsertPosition <= minimumLength =
        editor {insertPosition = insertPosition + moveAmount}
    | otherwise = editor
    where
        newInsertPosition = insertPosition + moveAmount
        -- don't use length bufferData as evaluates everything there to work out length
        -- could have a huge file!
        minimumLength = length (take newInsertPosition bufferData)

