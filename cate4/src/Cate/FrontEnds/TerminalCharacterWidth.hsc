module FrontEnds.TerminalCharacterWidth (getTerminalCharacterWidth) where

#include <wchar.h>

foreign import ccall "sys/wchar.h" wcwidth :: Char -> Int

getTerminalCharacterWidth :: Char -> Integer
getTerminalCharacterWidth = fromIntegral . wcwidth