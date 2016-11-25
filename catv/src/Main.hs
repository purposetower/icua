module Main where

import LayoutText

main :: IO ()
main = do
    putStrLn $ layoutText "123\n123\n123" (DisplaySize 5 5) (NoWrap 1) (NoPad)
