module Core.Types.CoreTypes where

data TerminalWindowSize = TerminalWindowSize {width :: Integer, height :: Integer}

data DisplaySection = DisplaySection {start :: Integer, end :: Integer, text :: String} deriving Show