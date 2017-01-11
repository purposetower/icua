module Core.Types.DisplaySize (DisplaySize(..)) where

-- | The size of the display to layout text on
data DisplaySize = DisplaySize {width :: Integer, height :: Integer}
