module Cate.Types.BufferTypes where

data Buffer = Buffer {cursors :: [Cursor], bufferStartPosition :: Int, bufferData :: String}

data Cursor = SimpleCursor {cursorPosition :: Int} | SelectionCursor {cursorStartPosition :: Int,
    cursorEndPosition :: Int}

-- TODO FINISH
instance Eq Cursor where
    (SimpleCursor firstCursorPosition) == (SimpleCursor secondCursorPosition) =
        firstCursorPosition == secondCursorPosition

-- TODO FINISH
instance Ord Cursor where
    compare (SimpleCursor firstCursorPosition) (SimpleCursor secondCursorPosition) =
        compare firstCursorPosition secondCursorPosition
