module Cate.Types.DisplayTypes where

data TerminalWindowSize = TerminalWindowSize {width :: Int, height :: Int}

data Display = Display {terminalWindowSize :: TerminalWindowSize, displayStartPosition :: Int}

data BufferSection = BufferSection {sectionStartPosition :: Int, sectionEndPosition :: Int} deriving (Show)

instance Eq BufferSection where
    (BufferSection firstSectionStartPosition firstSectionEndPosition) == (BufferSection secondSectionStartPosition secondSectionEndPosition) =
        firstSectionStartPosition == secondSectionStartPosition && firstSectionEndPosition == secondSectionEndPosition