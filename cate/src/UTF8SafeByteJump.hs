module UTF8SafeByteJump (safeByteJump, SafeByteJumpDirection(..)) where 

import System.IO (Handle, hSeek, SeekMode (AbsoluteSeek, RelativeSeek), hIsEOF, hGetChar)
import Data.ByteString.Lazy as BS
import Data.Word

data SafeByteJumpDirection = Backward | Forward

safeByteJump :: Handle
             -> Integer -- handle position
             -> SafeByteJumpDirection
             -> IO Integer -- return a safe handle position which starts at beginning of character

safeByteJump handle handlePosition safeByteJumpDirection =
    if handlePosition <= 0 then
        return 0
    else
        do
            hSeek handle AbsoluteSeek handlePosition

            currentByteString <- BS.hGet handle 1
            let currentWord8 = BS.head currentByteString
            -- if byte is between 128 and 191 then it is a continuation byte
            if currentWord8 >= 128 && currentWord8 <= 191 then
                case safeByteJumpDirection of
                    Backward -> safeByteJump handle (handlePosition - 1) Backward -- go back 1 and try again
                    Forward -> safeByteJump handle (handlePosition + 1) Forward -- go forward 1 and try again
            else
                do
                    hSeek handle RelativeSeek (-1)
                    return handlePosition

        
