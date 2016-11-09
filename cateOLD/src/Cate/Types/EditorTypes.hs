module Cate.Types.EditorTypes where

import Cate.Types.BufferTypes (Buffer)
import Cate.Types.DisplayTypes (Display)

-- how many bytes the user can paste in
defaultInputBufferByteSize :: Int
defaultInputBufferByteSize = 1024

data Editor = Editor {inputBufferByteSize :: Int, buffer :: Buffer, display :: Display}