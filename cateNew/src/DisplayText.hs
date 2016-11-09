module DisplayText (getDisplayText, DisplaySize(..), getDisplayTextWithPadding) where

import Prelude hiding (getLine)
import Data.List.Split

data DisplaySize = DisplaySize {width :: Integer, height :: Integer} deriving Show


getDisplayText :: String -> DisplaySize -> String

getDisplayText input (DisplaySize width height)
    -- no room on display
    | width <= 0 || height <= 0 = ""
    
    | otherwise = foldr (++) "" lines
    where
        lines = take (fromIntegral height) (getLines input width)


getLines :: String -> Integer -> [String]

getLines [] _ = []

getLines input width = line : getLines leftOverInput width
    where
        (line, leftOverInput) = getLine input width


getLine :: String -- input
        -> Integer -- width left
        -> (String, String) -- (line, left over input)

getLine [] _ = ([], [])

getLine (x:xs) width
    | x == '\n' = (['\n'], xs)

    | width == 0 = (['\n'], x:xs)

    | otherwise = (x : line, leftOverInput)

    where
        (line, leftOverInput) = getLine xs (width - 1)


-- Instead of clearing whole screen with white space and then printing, we add padding to our lines
-- TODO: smarter way of doing this...
getDisplayTextWithPadding :: String -> DisplaySize -> String
getDisplayTextWithPadding input displaySize@(DisplaySize width height) = joined
    where
        displayText = take (fromIntegral height) $ splitOn "\n" $ getDisplayText input displaySize
        padded = map (\x -> x ++ padding (width - (toInteger $ length x))) displayText
        emptyLines = take ((fromIntegral height) - length padded) $ repeat $ padding width
        -- remove last new line
        joined = init $ foldr (++) "" (padded ++ emptyLines)


padding :: Integer -> String
padding width = (foldr (++) "" $ take (fromIntegral width) $ repeat " ") ++ "\n"
