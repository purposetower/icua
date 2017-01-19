module Core.Server where

import Network
import System.IO (hSetBuffering, BufferMode(..), hGetLine, hPutStrLn)
import Control.Concurrent (forkIO, threadDelay)
import System.IO (Handle)

main :: IO ()
main = do
    socket <- listenOn $ PortNumber 5555
    socketHandler socket

socketHandler :: Socket -> IO ()
socketHandler socket = do
    (handle, hostName, portNumber) <- accept socket
    hSetBuffering handle NoBuffering
    forkIO $ proccess handle
    socketHandler socket

proccess :: Handle -> IO ()
proccess handle = do
    line <- hGetLine handle
    putStrLn line
    hPutStrLn handle "hello from server"
    proccess handle