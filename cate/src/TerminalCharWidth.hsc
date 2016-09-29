module TerminalCharWidth (getTerminalCharWidth) where

#include <wchar.h>

foreign import ccall "sys/wchar.h" wcwidth :: Char -> Int

getTerminalCharWidth :: Char -> Integer
getTerminalCharWidth = fromIntegral . wcwidth