module TerminalType where

import System.IO (Handle)

data Terminal = Terminal {inputBufferByteSize :: Int, handle :: Handle, handlePosition :: Integer, leftMarginDisplayOffset :: Integer}