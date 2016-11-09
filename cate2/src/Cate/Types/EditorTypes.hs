module Cate.Types.EditorTypes where

import System.IO (Handle)

data TerminalWindowSize = TerminalWindowSize {width :: Int, height :: Int}

-- how many bytes the user can paste in
defaultInputPasteByteSize :: Int
defaultInputPasteByteSize = 1024

-- how much of the file we read in
defaultMaxBufferSize :: Int
defaultMaxBufferSize = 30000

data Editor = Editor {inputPasteByteSize :: Int, maxBufferSize :: Int, terminalWindowSize :: TerminalWindowSize,
    originalFileHandle :: Handle, displaySections :: [DisplaySection]}

data DisplaySection = DisplaySection {sectionStartPosition :: Int, sectionEndPosition :: Int, text :: String} deriving (Show)