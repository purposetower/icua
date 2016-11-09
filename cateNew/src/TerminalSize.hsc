module TerminalSize (getDisplaySize) where

import DisplayText (DisplaySize(..))

import Foreign
import Foreign.C.Types
import System.Posix.Types (Fd(Fd))
import System.Posix.IO (stdOutput)

#include <sys/ioctl.h>
#include <unistd.h>


data TerminalSizeStruct = TerminalSizeStruct CUShort CUShort


instance Storable TerminalSizeStruct where
  sizeOf _ = (#size struct winsize)

  alignment _ = (#alignment struct winsize)

  peek ptr = do
    col <- (#peek struct winsize, ws_col) ptr
    row <- (#peek struct winsize, ws_row) ptr
    return $ TerminalSizeStruct col row

  poke _ _ = return () -- don't do anything


foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr TerminalSizeStruct -> IO CInt


getDisplaySize :: IO DisplaySize
getDisplaySize = with (TerminalSizeStruct 0 0) $ \terminalSizeStruct -> do
    _ <- ioctl (stdOutputCInt stdOutput) (#const TIOCGWINSZ) terminalSizeStruct
    TerminalSizeStruct col row <- peek terminalSizeStruct
    return $ DisplaySize (fromIntegral col) (fromIntegral row)


stdOutputCInt :: Fd -> CInt
stdOutputCInt (Fd fd) = fd
