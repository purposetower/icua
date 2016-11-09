module Main where

import TerminalSize (getDisplaySize)
import DisplayText (getDisplayText, WrapMode (Wrap, NoWrap))


main :: IO ()
main = do
    displaySize <- getDisplaySize
    putStr $ getDisplayText "Some data long lineeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeaa\nMore data" displaySize (NoWrap 2) True
