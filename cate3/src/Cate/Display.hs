module Cate.Display (start, display, shutdown) where

import Cate.ANSICodes
import Cate.ASCIICodes
import Cate.Types.EditorTypes

import System.IO (hFlush, stdout)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Terminal (TerminalAttributes, TerminalMode (EnableEcho, ExtendedFunctions,
    KeyboardInterrupts, ProcessInput, StartStopOutput), TerminalState (Immediately),
    getTerminalAttributes, setTerminalAttributes, withoutMode)

start :: IO TerminalAttributes
start = do
    -- save terminal attributes
    originalTerminalAttributes <- getTerminalAttributes stdInput
    originalOutputTerminalAttributes <- getTerminalAttributes stdOutput

    -- put the terminal in "raw mode"(let us handle escape sequences)
    let rawTerminalAttributes = foldl withoutMode originalTerminalAttributes
            [ProcessInput, EnableEcho, KeyboardInterrupts, StartStopOutput
            , ExtendedFunctions]
    setTerminalAttributes stdInput rawTerminalAttributes Immediately

    return originalTerminalAttributes

display :: [DisplaySection] -> IO ()
display displaySections = do 
    putStr $ setCursorPositionCode (0, 0) ++ clearScreenCode ++ showDisplaySections displaySections
    -- print immediatly
    hFlush stdout

showDisplaySections :: [DisplaySection] -> String
showDisplaySections [] = []
showDisplaySections (x:xs) = (text x) ++ showDisplaySections xs

shutdown :: TerminalAttributes -> IO ()
shutdown originalTerminalAttributes = do
    -- add a new line
    putStrLn ""
    -- print immediatly
    hFlush stdout
    -- reset original terminal
    setTerminalAttributes stdInput originalTerminalAttributes Immediately
    return ()