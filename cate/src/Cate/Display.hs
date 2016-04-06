module Cate.Display(calculateBufferSections,createDisplay, displayEditor, insertIntoEditor, deleteFromEditor,
    moveInsertPositionHorizontally, moveInsertPositionVertically, moveStartPosition, moveStartPositionToShowLastCursor) where

import Cate.ANSICodes
import Cate.ASCIICodes
import Cate.TerminalSize
import Cate.Buffer
import Cate.Types.DisplayTypes (TerminalWindowSize (..), Display (..), BufferSection (..))
import Cate.Types.EditorTypes (Editor (..))
import Cate.Types.BufferTypes (Buffer (..), Cursor (..))

import Data.List
import System.IO (IOMode (ReadMode), hFlush, hGetContents, openFile, stdout)


createDisplay :: IO Display
createDisplay = do
    terminalWindowSize <- getTerminalWindowSize
    return (Display terminalWindowSize 0)

displayEditor :: Editor -> IO ()
displayEditor (Editor _ (Buffer _ bufferStartPosition bufferData)
    (Display terminalWindowSize _)) = do
        putStr $ hideCursor ++ clearScreenCode ++ setCursorPositionCode (0, 0) ++
            getDisplayFromBufferSections bufferStartPosition terminalWindowSize bufferData
        -- print immediatly
        hFlush stdout

insertIntoEditor :: String -> Editor -> Editor
insertIntoEditor input editor@(Editor _ oldBuffer _) =
    moveInsertPositionHorizontally 1 (editor {buffer = insertIntoBuffer input oldBuffer})

deleteFromEditor :: Editor -> Editor
deleteFromEditor editor@(Editor _ oldBuffer _) = editor {buffer = deleteFromBuffer oldBuffer}

moveInsertPositionHorizontally :: Int -> Editor -> Editor
moveInsertPositionHorizontally moveAmount editor@(Editor _ oldBuffer _) =
    editor {buffer = moveBufferInsertPosition moveAmount oldBuffer}

moveInsertPositionVertically :: Int -> Editor -> Editor
moveInsertPositionVertically moveAmount editor = editor

moveStartPosition :: Int -> Editor -> Editor
moveStartPosition moveAmount editor@(Editor _ oldBuffer _) =
    editor {buffer = moveBufferStartPosition moveAmount oldBuffer}

moveStartPositionToShowLastCursor :: Editor -> Editor
moveStartPositionToShowLastCursor editor@(Editor _ oldBuffer@(Buffer cursors bufferStartPosition bufferData)
    (Display (TerminalWindowSize width height) _)) =
    editor {buffer = oldBuffer {bufferStartPosition = newBufferStartPosition}}
    where
        cursorPosition = case last cursors of
            SimpleCursor cursorPosition -> cursorPosition
            SelectionCursor _ cursorEndPosition -> cursorEndPosition
        bufferSections = calculateBufferSections 0 width bufferData
        cursorBufferSection = head [x | x <- bufferSections,
            cursorPosition >= sectionStartPosition x && cursorPosition <= sectionEndPosition x]
        cursorBufferSectionIndex = (case (elemIndex cursorBufferSection bufferSections) of
            Just c -> c
            Nothing -> 0)
        heightWithLastCheck = if cursorPosition - 1 >= 0 && bufferData !! (cursorPosition - 1) == newLine then height - 2 else height - 1
        newBufferStartPosition = sectionStartPosition (head (drop (cursorBufferSectionIndex
            - heightWithLastCheck) bufferSections))


getDisplayFromBufferSections :: Int -> TerminalWindowSize -> String -> String
getDisplayFromBufferSections _ _ [] = []

getDisplayFromBufferSections sectionStartPosition (TerminalWindowSize width height) bufferData
    | bufferSectionData == [] = []
    | otherwise = if last bufferSectionData == newLine then init bufferSectionData else bufferSectionData
    where
        bufferSections = take height (calculateBufferSections sectionStartPosition width (drop sectionStartPosition bufferData))
        bufferSectionData = foldl1 (++) (map (getDataFromBufferSection bufferData) bufferSections)

getDataFromBufferSection :: String -> BufferSection -> String
getDataFromBufferSection bufferData (BufferSection sectionStartPosition sectionEndPosition) =
    drop sectionStartPosition $ take sectionEndPosition bufferData

calculateBufferSections :: Int -> Int -> String -> [BufferSection]
calculateBufferSections _ _ [] = []

calculateBufferSections sectionStartPosition maxWidth bufferData =
    BufferSection sectionStartPosition sectionEndPosition :
        calculateBufferSections sectionEndPosition maxWidth (drop endPositionOffset bufferData)
    where
        newLineIndex = elemIndex newLine (take maxWidth bufferData)
        endPositionOffset = case newLineIndex of
            Just x -> if x < maxWidth then x + 1 else maxWidth
            Nothing -> length $ take maxWidth bufferData
        sectionEndPosition = sectionStartPosition + endPositionOffset

