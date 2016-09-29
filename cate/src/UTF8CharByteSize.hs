module UTF8CharByteSize (getUTF8CharByteSize) where

import Codec.Binary.UTF8.String

-- get the number of bytes a utf8 character takes
getUTF8CharByteSize :: Char -> Integer
getUTF8CharByteSize = toInteger . length . encodeChar