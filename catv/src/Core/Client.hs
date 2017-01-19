module Core.Client where

import Network
import System.IO (hSetBuffering, BufferMode(..), hGetLine, hPutStrLn)
import Control.Concurrent (threadDelay)
import System.IO (Handle)
import System.Random

main :: IO ()
main = do
    handle <- connectTo "localhost" $ PortNumber 5555
    randomInt <- randomIO :: IO Int
    proccess handle randomInt
    
proccess :: Handle -> Int -> IO ()
proccess handle randomInt = do
    hPutStrLn handle $ "hello from client" ++ (show randomInt)
    line <- hGetLine handle
    putStrLn line
    threadDelay (2000000)
    proccess handle randomInt