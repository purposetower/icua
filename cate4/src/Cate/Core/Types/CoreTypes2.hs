module Core.Types.CoreTypes2 where

-- a single row of our display
data DisplayRow = DisplayRow {

    start :: Integer, -- byte start position pointer of display section
    end :: Integer, -- byte end position pointer of display section
    text :: String -- text data making up display section

} deriving Show