module Cate.TerminalSize (getTerminalWindowSize) where

import Cate.Types.DisplayTypes (TerminalWindowSize(..))

import Foreign
import Foreign.C.Types
import System.Posix.Types (Fd(Fd))
import System.Posix.IO (stdOutput)

#include <sys/ioctl.h>
#include <unistd.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data TerminalSizeStruct = TerminalSizeStruct CUShort CUShort

instance Storable TerminalSizeStruct where
  sizeOf _ = (#size struct winsize)
  
  alignment _ = (#alignment struct winsize)
  
  peek ptr = do
    col <- (#peek struct winsize, ws_col) ptr
    row <- (#peek struct winsize, ws_row) ptr
    return $ TerminalSizeStruct col row
    
  poke ptr (TerminalSizeStruct col row) = do
    (#poke struct winsize, ws_col) ptr col
    (#poke struct winsize, ws_row) ptr row

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr TerminalSizeStruct -> IO CInt

getTerminalWindowSize :: IO TerminalWindowSize
getTerminalWindowSize = with (TerminalSizeStruct 0 0) $ \ts -> do
    ioctl (stdOutputCInt stdOutput) (#const TIOCGWINSZ) ts
    TerminalSizeStruct row col <- peek ts
    return $ TerminalWindowSize (fromIntegral row) (fromIntegral col)

stdOutputCInt (Fd fd) = fd
