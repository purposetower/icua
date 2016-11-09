module Cate.TerminalCharacterWidth (getTerminalCharacterWidth) where

#include <wchar.h>

foreign import ccall "sys/wchar.h" wcwidth :: Char -> Int

getTerminalCharacterWidth :: Char -> Int
getTerminalCharacterWidth = wcwidth