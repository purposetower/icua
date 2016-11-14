module DisplayText (getDisplayText, DisplaySize(..), WrapMode(..)) where

import Prelude hiding (getLine)


data DisplaySize = DisplaySize {width :: Integer, height :: Integer} deriving Show

data WrapMode =
    Wrap |
    NoWrap { leftMarginDisplayOffset :: Integer } -- how far we are from the left margin


getDisplayText :: String -> DisplaySize -> WrapMode -> Bool -> String
getDisplayText input (DisplaySize width height) wrapMode padded
    -- no room on display
    | width <= 0 || height <= 0 = ""
    
    | otherwise = result
    where
        lines = take (fromIntegral height)
            (case wrapMode of
                Wrap -> getLinesWrap input width padded
                NoWrap leftMarginOffset -> getLinesNoWrap input width leftMarginOffset padded)

        -- used if we are padding the output
        emptyLines = foldr (++) "" $ take ((fromIntegral height) - length lines) $
            repeat $ linePadding width

        concatWithEmpty = concatLines lines ++ emptyLines

        -- check if last character is a newline, only for padded
        result = if padded
            then
                if concatWithEmpty /= [] && last concatWithEmpty == '\n'
                    then init concatWithEmpty
                    else concatWithEmpty
            else
                concatLines lines


getLinesWrap :: String -> Integer -> Bool -> [String]
getLinesWrap [] _ _ = []

getLinesWrap input width padded = line : getLinesWrap leftOverInput width padded
    where
        (line, leftOverInput) = getLine input width padded


getLinesNoWrap :: String -> Integer -> Integer -> Bool -> [String]
getLinesNoWrap [] _ _ _ = []

getLinesNoWrap input width leftMarginDisplayOffset padded =
    line : getLinesNoWrap leftOverInputNoWrap width leftMarginDisplayOffset padded
    where
        inputAfterLeftMarginApplied = dropTillNewLine leftMarginDisplayOffset input

        (line, leftOverInput) = getLine inputAfterLeftMarginApplied width padded

        -- skip non viewable text when in no wrap mode
        leftOverInputNoWrap = if checkLineEndsWithNewLine line
            then leftOverInput
            else removeCharactersUpToNewLine leftOverInput


getLine :: String -- input
        -> Integer -- width left
        -> Bool -- should we pad the line?
        -> (String, String) -- (line, left over input)
getLine [] _ _ = ([], [])

getLine (x:xs) width padded
    | x == '\n' = if padded then (linePadding width, xs) else ("\n", xs)

    | width == 0 = ([], x:xs)

    | otherwise = (x : line, leftOverInput)
    where
        (line, leftOverInput) = getLine xs (width - 1) padded


linePadding :: Integer -> String
linePadding width = (foldr (++) "" $ take (fromIntegral width) $ repeat " ") ++ "\n"


checkLineEndsWithNewLine :: String -> Bool
checkLineEndsWithNewLine [] = False

checkLineEndsWithNewLine "\n" = True

checkLineEndsWithNewLine (x:xs) = checkLineEndsWithNewLine xs


removeCharactersUpToNewLine :: String -> String
removeCharactersUpToNewLine [] = []

removeCharactersUpToNewLine ('\n' : xs) = xs

removeCharactersUpToNewLine (x:xs) = removeCharactersUpToNewLine xs


concatLines :: [String] -> String
concatLines [] = []

concatLines (x:[]) = x -- don't add new line

-- make sure we add new lines when concatenating
concatLines (x:xs) = endWithNewLine x ++ concatLines xs


endWithNewLine :: String -> String
endWithNewLine line = if checkLineEndsWithNewLine line then line else line ++ "\n"


dropTillNewLine :: Integer -> String -> String
dropTillNewLine amount input@(x:xs)
    | amount <= 0 = input
    | input == [] = []
    | x == '\n' = input
    | otherwise = dropTillNewLine (amount - 1) xs
