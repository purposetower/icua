module UI.Terminal.TerminalRawMode (putTerminalInRawMode, resetTerminalFromRawMode) where

import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Terminal (TerminalAttributes, TerminalMode (EnableEcho, ExtendedFunctions,
    KeyboardInterrupts, ProcessInput, StartStopOutput), TerminalState (Immediately),
    getTerminalAttributes, setTerminalAttributes, withoutMode)


putTerminalInRawMode :: IO TerminalAttributes
putTerminalInRawMode = do
    originalTerminalAttributes <- getTerminalAttributes stdInput
    originalOutputTerminalAttributes <- getTerminalAttributes stdOutput

    -- put the terminal in "raw mode"(let us handle escape sequences)
    let rawTerminalAttributes = foldl withoutMode originalTerminalAttributes
            [ProcessInput, EnableEcho, KeyboardInterrupts, StartStopOutput
            , ExtendedFunctions]
    setTerminalAttributes stdInput rawTerminalAttributes Immediately

    return originalTerminalAttributes


resetTerminalFromRawMode :: TerminalAttributes -> IO ()
resetTerminalFromRawMode originalTerminalAttributes = do
    setTerminalAttributes stdInput originalTerminalAttributes Immediately
    return ()
