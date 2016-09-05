module TerminalCharacterByteSize where

import Codec.Binary.UTF8.String

-- get the number of bytes a utf8 character takes
getCharacterByteSize :: Char -> Integer
getCharacterByteSize = toInteger . length . encodeChar