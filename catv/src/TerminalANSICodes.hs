module TerminalANSICodes (setCursorPositionCode, hideCursor, showCursor, cursorUp, cursorDown,
    cursorRight, cursorLeft) where

-- ANSI escape codes, "\ESC[" + numbers seperated by semicolons + some string
csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ addSemiColonToCsi (map show args) ++ code

-- add ; to csi, pass in ["2", "3"] return ["2;3"]
addSemiColonToCsi :: [String] -> String
addSemiColonToCsi [] = ""
addSemiColonToCsi [x] = x
addSemiColonToCsi (x:xs) = x ++ ";" ++ addSemiColonToCsi xs

setCursorPositionCode (n, m) = csi [n + 1, m + 1] "H"

hideCursor = csi [] "?25l"
showCursor = csi [] "?25h"

cursorUp = csi [] "A"
cursorDown = csi [] "B"
cursorRight = csi [] "C"
cursorLeft = csi [] "D"
