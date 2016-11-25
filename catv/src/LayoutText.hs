module LayoutText (layoutText, getLinesWrap, getLinesNoWrap, DisplaySize(..), WrapMode(..), PadMode(..)) where

-- name clash
import Prelude hiding (getLine)

-- | The size of the display to layout text on
data DisplaySize = DisplaySize {width :: Integer, height :: Integer}

-- | What to do if a line of text is larger than display
data WrapMode =
    Wrap |
    NoWrap { leftMargin :: Integer } -- ^ The display offset from the left margin

data PadMode =
    Pad | -- ^ Add blank spaces " "
    NoPad -- ^ Return text as is


-- | Layout the input text so that it fits onto our display
layoutText :: String -> DisplaySize -> WrapMode -> PadMode -> String
layoutText input displaySize@(DisplaySize width height) wrapMode padMode
    | width <= 0 || height <= 0 = []

    | otherwise = result
    where
        lines = case wrapMode of
            Wrap -> getLinesWrap input displaySize
            NoWrap leftMargin -> getLinesNoWrap input leftMargin displaySize

        result = case padMode of
            -- remove last new line or we will overflow display
            Pad -> init $ linesPad lines width height
            NoPad -> linesNoPad lines


-- | Text we want to display when in wrap mode
getLinesWrap :: String -> DisplaySize -> [String]
getLinesWrap input (DisplaySize width height) = take (fromIntegral height) $ getLines input width lineWrap

-- | Text we want to display when in no wrap mode
getLinesNoWrap :: String -> Integer -> DisplaySize -> [String]
getLinesNoWrap input leftMargin (DisplaySize width height) = take (fromIntegral height) $
    -- apply left margin to first line
    getLines (dropTillNewLine leftMargin input) width (lineNoWrap leftMargin)


getLines :: String -> Integer -> (String -> String -> String) -> [String]
getLines [] _ _ = []

getLines input width leftOverTransform =
    line : getLines newLeftOverInput width leftOverTransform
    where
        (line, leftOverInput) = getLine input width
        newLeftOverInput = leftOverTransform line leftOverInput


getLine :: String -> Integer -> (String, String)
getLine [] _ = ([], [])

getLine (x:xs) width
    | x == '\n' = ("\n", xs)

    | width == 0 = ([], x:xs)

    | otherwise = (x : line, leftOverInput)
    where
        (line, leftOverInput) = getLine xs (width - 1)

-------------------------------------------------------------------------------
-- Line wrapping
-------------------------------------------------------------------------------

lineWrap :: String -> String -> String
lineWrap line leftOverInput = leftOverInput -- don't do anything

lineNoWrap :: Integer -> String -> String -> String
lineNoWrap leftMargin line leftOverInput =
    -- apply left margin, ignore characters past left of display
    dropTillNewLine leftMargin (case line of
        [] -> leftOverInput
        
        _ -> if last line == '\n'
            -- next line of display
            then leftOverInput
            -- ignore characters past right of display
            else removeUpToNewLine leftOverInput)

dropTillNewLine :: Integer -> String -> String
dropTillNewLine _ [] = []

dropTillNewLine amount input@(x:xs)
    | amount <= 0 = input
    | x == '\n' = input
    | otherwise = dropTillNewLine (amount - 1) xs

removeUpToNewLine :: String -> String
removeUpToNewLine [] = []
removeUpToNewLine ('\n' : xs) = xs
removeUpToNewLine (x:xs) = removeUpToNewLine xs

-------------------------------------------------------------------------------
-- Line padding
-------------------------------------------------------------------------------

-- Add padding the every line followed by a new line character
linesPad :: [String] -> Integer -> Integer -> String
linesPad [] _ 0 = []

-- add an empty line
linesPad [] width height = padding width ++ linesPad [] width (height - 1)
-- add an empty line
linesPad ([]:xs) width height = padding width ++ linesPad [] width (height - 1)

linesPad (x:xs) width height =
    xWithoutNewLine ++ padding paddingAmount ++ linesPad xs width (height - 1)
    where
        xWithoutNewLine = if last x == '\n' then init x else x
        paddingAmount = width - (toInteger $ length xWithoutNewLine)

padding :: Integer -> String
padding width = (foldr (++) "" $ take (fromIntegral width) $ repeat " ") ++ "\n"


-- Add the new line character to the end of every line except the last
linesNoPad :: [String] -> String
linesNoPad [] = []

-- don't add a new line to last line, we leave the new line if it's in there though
linesNoPad (x:[]) = x

linesNoPad ([]:xs) = linesNoPad xs

linesNoPad (x:xs) =
    (if last x == '\n' then x else x ++ "\n") ++ linesNoPad xs
