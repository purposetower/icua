module Cate.Buffer (createBuffer, insertIntoBuffer, deleteFromBuffer, moveBufferStartPosition,
    moveBufferInsertPosition, addBufferCursor, removeBufferCursors) where

import Cate.Types.BufferTypes (Buffer (..), Cursor (..))

import Data.List
import System.IO (IOMode (ReadMode), hGetContents, openFile)

createBuffer :: [String] -> IO Buffer
-- empty buffer
createBuffer [] = return (Buffer [SimpleCursor 0] 0 "")

-- load file into buffer
createBuffer (x:xs) = do
    -- ignore everything after first arg
    fileHandle <- openFile x ReadMode
    fileContents <- hGetContents fileHandle
    return (Buffer [SimpleCursor 0] 0 fileContents)

insertIntoBuffer :: String -> Buffer -> Buffer
insertIntoBuffer input (Buffer cursors@(firstCursor : xs) bufferStartPosition bufferData) =
    Buffer (map convertSelectionCursorToSimpleCursor cursors) bufferStartPosition
        ((case firstCursor of
            SimpleCursor cursorPosition -> take cursorPosition bufferData
            SelectionCursor cursorStartPosition _ -> take cursorStartPosition bufferData) ++
        insertIntoBufferCursors input bufferData cursors)

-- convert SimpleCursors into SelectionCursor one away from cursorPosition
-- then add "" into those
deleteFromBuffer :: Buffer -> Buffer
deleteFromBuffer (Buffer cursors@(SimpleCursor cursorPosition : xs) bufferStartPosition bufferData) =
    Buffer (map convertSelectionCursorToSimpleCursor deleteCursors) bufferStartPosition
        (take (cursorPosition - 1) bufferData ++ insertIntoBufferCursors "" bufferData deleteCursors)
    where
        deleteCursors = map convertSimpleCursorToDeleteSelectionCursor cursors

deleteFromBuffer (Buffer cursors@(SelectionCursor cursorStartPosition _ : xs)
    bufferStartPosition bufferData) =
        Buffer (map convertSelectionCursorToSimpleCursor deleteCursors) bufferStartPosition
            (take cursorStartPosition bufferData ++ insertIntoBufferCursors "" bufferData deleteCursors)
        where
            deleteCursors = map convertSimpleCursorToDeleteSelectionCursor cursors

-- move position we start rendering from
moveBufferStartPosition :: Int -> Buffer -> Buffer
moveBufferStartPosition moveAmount (Buffer cursors bufferStartPosition bufferData) =
    Buffer cursors (if newBufferStartPosition >= 0 && newBufferStartPosition <= maxPosition
        then
            newBufferStartPosition
        else
            bufferStartPosition) bufferData
    where
        newBufferStartPosition = bufferStartPosition + moveAmount
        -- don't use length bufferData as evaluates everything there, could be a huge file!
        maxPosition = length (take newBufferStartPosition bufferData)

-- move positions we start inserting from
moveBufferInsertPosition :: Int -> Buffer -> Buffer
moveBufferInsertPosition moveAmount (Buffer cursors bufferStartPosition bufferData) =
    Buffer (moveBufferCursors moveAmount bufferData cursors) bufferStartPosition bufferData

moveBufferCursors :: Int -> String -> [Cursor] -> [Cursor]
moveBufferCursors _ _ [] = []

moveBufferCursors moveAmount bufferData (SimpleCursor cursorPosition:xs) = newCursor :
    moveBufferCursors moveAmount bufferData xs
    where
        newCursorPosition = cursorPosition + moveAmount
        -- don't use length bufferData as evaluates everything there, could be a huge file!
        maxPosition = length (take newCursorPosition bufferData)
        newCursor = if newCursorPosition >= 0 && newCursorPosition <= maxPosition
            then
                SimpleCursor newCursorPosition
            else
                SimpleCursor cursorPosition

moveBufferCursors moveAmount bufferData (SelectionCursor cursorStartPosition cursorEndPosition:xs) =
    newCursor : moveBufferCursors moveAmount bufferData xs
    where
        newCursorPosition = if moveAmount > 0 then cursorEndPosition else cursorStartPosition
        newCursor = SimpleCursor newCursorPosition

insertIntoBufferCursors :: String -> String -> [Cursor] -> String

insertIntoBufferCursors input bufferData [cursor] = input ++ case cursor of
    SimpleCursor cursorPosition -> drop cursorPosition bufferData
    SelectionCursor _ cursorEndPosition -> drop cursorEndPosition bufferData

insertIntoBufferCursors input bufferData (firstCursor : secondCursor : xs) = input ++
    case firstCursor of
        SimpleCursor cursorPositionFirst -> case secondCursor of

            SimpleCursor cursorPositionSecond ->
                drop cursorPositionFirst $ take cursorPositionSecond bufferData

            SelectionCursor cursorStartPositionSecond _ ->
                drop cursorPositionFirst $ take cursorStartPositionSecond bufferData

        SelectionCursor cursorStartPositionFirst cursorEndPositionFirst -> case secondCursor of

            SimpleCursor cursorPositionSecond ->
                drop cursorStartPositionFirst $ take cursorEndPositionFirst $
                    take cursorPositionSecond bufferData
            
            SelectionCursor cursorStartPositionSecond _ ->
                drop cursorStartPositionFirst $ take cursorEndPositionFirst $
                    take cursorStartPositionSecond bufferData
    
    ++ insertIntoBufferCursors input bufferData (secondCursor : xs)


convertSelectionCursorToSimpleCursor :: Cursor -> Cursor
convertSelectionCursorToSimpleCursor simpleCursor@(SimpleCursor _) = simpleCursor

convertSelectionCursorToSimpleCursor (SelectionCursor cursorStartPosition _) =
    SimpleCursor cursorStartPosition

convertSimpleCursorToDeleteSelectionCursor :: Cursor -> Cursor
convertSimpleCursorToDeleteSelectionCursor selectionCursor@(SelectionCursor _ _) = selectionCursor

convertSimpleCursorToDeleteSelectionCursor simpleCursor@(SimpleCursor cursorPosition)
    | cursorPosition - 1 >= 0 = SelectionCursor (cursorPosition - 1) cursorPosition
    | otherwise = simpleCursor

-- add a cursor into an ordered cursors list
addBufferCursor :: Cursor -> Buffer -> Buffer
addBufferCursor cursor (Buffer cursors bufferStartPosition bufferData) =
    Buffer (insert cursor cursors) bufferStartPosition bufferData

removeBufferCursors :: Buffer -> Buffer
removeBufferCursors (Buffer cursors bufferStartPosition bufferData) =
    Buffer [] bufferStartPosition bufferData
