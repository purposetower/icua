module Core.LayoutText (getLinesWrap, getLinesNoWrap) where

-- name clash
import Prelude hiding (getLine)

import Core.Types.DisplaySize


-- | Text we want to display when in wrap mode
getLinesWrap :: String -> DisplaySize -> [String]
getLinesWrap input (DisplaySize width height) = take (fromIntegral height) $ getLines input width lineWrap


-- | Text we want to display when in no wrap mode
getLinesNoWrap :: String -> Integer -> DisplaySize -> [String]
getLinesNoWrap input leftMargin (DisplaySize width height) = take (fromIntegral height) $
    -- apply left margin to first line
    getLines (dropTillNewLine leftMargin input) width (lineNoWrap leftMargin)


-- | Accumulate lines, pass in function which handles lines wider than display
getLines :: String -> Integer -> (String -> String -> String) -> [String]
getLines [] _ _ = []

getLines input width leftOverTransform =
    (dropLastNewLine line) : getLines newLeftOverInput width leftOverTransform
    where
        (line, leftOverInput) = getLine input width
        newLeftOverInput = leftOverTransform line leftOverInput


getLine :: String -> Integer -> (String, String)
getLine [] _ = ([], [])

getLine (x:xs) width
    -- we leave the newline in as this affects further processing
    | x == '\n' = ("\n", xs)

    | width == 0 = ([], x:xs)

    | otherwise = (x : line, leftOverInput)
    where
        (line, leftOverInput) = getLine xs (width - 1)

-------------------------------------------------------------------------------
-- Line wrapping
-------------------------------------------------------------------------------

lineWrap :: String -> String -> String
lineWrap _ leftOverInput = leftOverInput -- don't do anything


lineNoWrap :: Integer -> String -> String -> String
lineNoWrap leftMargin line leftOverInput =
    -- apply left margin, ignoring characters past left of display
    dropTillNewLine leftMargin noWrapLeftOverInput
    where
        -- process left over input, if the previous line we received has a new
        -- line then we leave it, otherwise we keep dropping leftOverInput until
        -- we find a new line
        noWrapLeftOverInput = case line of
            [] -> leftOverInput
            _ -> if last line == '\n'
                    then leftOverInput
                    else removeUpToNewLine leftOverInput

-------------------------------------------------------------------------------
-- String utils
-------------------------------------------------------------------------------

dropLastNewLine :: String -> String
dropLastNewLine [] = []

dropLastNewLine input = if last input == '\n' then init input else input


dropTillNewLine :: Integer -> String -> String
dropTillNewLine _ [] = []

dropTillNewLine amount input@(x:xs)
    | amount <= 0 = input
    | x == '\n' = input
    | otherwise = dropTillNewLine (amount - 1) xs


removeUpToNewLine :: String -> String
removeUpToNewLine [] = []
removeUpToNewLine ('\n' : xs) = xs
removeUpToNewLine (_:xs) = removeUpToNewLine xs
