module Cate.Buffer where

import Cate.ANSICodes
import Cate.TerminalSize

data Buffer = Buffer {cursorPosition :: Int, bufferData :: String
                     , terminalWindowSize :: TerminalWindowSize}

createEmptyBuffer :: Buffer
createEmptyBuffer = Buffer 0 "" (TerminalWindowSize 0 0)

-- draw onto our terminal
displayBuffer :: Buffer -> IO Buffer
displayBuffer currentBuffer = do
    terminalWindowSize <- terminalSize
    -- hide cursor
    putStr hideCursor
    -- start printing from the top
    putStr $ setCursorPositionCode 0 0
    let newBuffer = sizeBuffer terminalWindowSize currentBuffer
    putStr $ bufferData newBuffer
    placeCursor terminalWindowSize (cursorPosition newBuffer)
    -- restore cursor
    putStr showCursor
    return newBuffer

-- copy and resize buffer
sizeBuffer :: TerminalWindowSize -> Buffer -> Buffer
sizeBuffer terminalWindowSize buffer = Buffer (cursorPosition buffer)
    (take bufferAmountToRead (bufferData buffer) ++
        generateBufferSpaces terminalWindowSize bufferAmountToRead)
        terminalWindowSize
    where
        bufferSize = length (bufferData buffer)
        tWSize = (width terminalWindowSize) * (height terminalWindowSize)
        bufferAmountToRead =
            if bufferSize <= tWSize then
                bufferSize
            else
                tWSize

-- TODO more efficient way to clear terminal?
generateBufferSpaces :: TerminalWindowSize -> Int -> String
generateBufferSpaces (TerminalWindowSize width height) bufferSize =
    if width * height > bufferSize then
        replicate (width * height - bufferSize) ' '
    else
        ""

placeCursor :: TerminalWindowSize -> Int -> IO ()
placeCursor (TerminalWindowSize width height) cursorPosition =
    -- height then width!
    putStr $ setCursorPositionCode row column
    where row = cursorPosition `div` width
          column = cursorPosition `rem` width

-- Add String to buffer and move cursor position
insertIntoBuffer :: Buffer -> String -> Buffer
insertIntoBuffer (Buffer cPosition bData tWSize) insertData =
    Buffer newCursorPosition (dataBeforeCursor ++ insertData ++ dataAfterCursor) 
        tWSize
    where
        dataLength = length insertData
        newCursorPosition = dataLength + cPosition
        dataBeforeCursor = take cPosition bData
        dataAfterCursor = drop (cPosition + dataLength) bData

-- remove characters from location
removeFromBuffer :: Buffer -> Int -> Int -> Buffer
removeFromBuffer (Buffer cPosition bData tWSize) from to =
    Buffer cPosition (dataBeforeLocation ++ dataAfterLocation) tWSize
    where
        dataBeforeLocation = take from bData
        dataAfterLocation = drop to bData

insertNewLineIntoBuffer :: Buffer -> Buffer
insertNewLineIntoBuffer buffer@(Buffer cPosition bData tWSize) =
    Buffer newCursorPosition newBufferData tWSize
    where
        lineLength = width tWSize
        cPositionWidth = cPosition `rem` lineLength
        amountOfSpaces = lineLength - cPositionWidth - 1
        newCursorPosition = cPosition + amountOfSpaces + 1
        dataBeforeCursor = take cPosition bData
        dataAfterCursor = drop cPosition bData
        newBufferData = dataBeforeCursor ++ (replicate amountOfSpaces ' ') ++
            "\n" ++ dataAfterCursor
