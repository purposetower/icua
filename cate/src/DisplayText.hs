{-# LANGUAGE DuplicateRecordFields #-}

module DisplayText (getDisplayRows, TextToDisplay(..), WrapMode(..), DisplayRow(..)) where

import Debug.Trace

-- a single row of our display
data DisplayRow = DisplayRow {
    start :: Integer, -- byte start position pointer of display section
    end :: Integer, -- byte end position pointer of display section
    text :: String -- text data making up display section
} deriving Show

-- text which we want to display
data TextToDisplay = TextToDisplay {
    text :: String,
    start :: Integer, -- display start position
    maxDisplayWidth :: Integer, -- max width of display
    getCharDisplayWidth :: (Char -> Integer), -- gets the width of a character on display (eg: east asian
                                       -- can have width 2, kana can have width 0.5)
    getCharBytesSize :: (Char -> Integer), -- gets the number of bytes character uses
    wrapMode :: WrapMode
}

data WrapMode =
    Wrap |
    NoWrap {
        leftMarginDisplayOffset :: Integer -- how far we are from the left margin
    }


-- Return the DisplayRows(the byte start position and byte end position pointers make up a display
-- row along with the actual text data)
getDisplayRows :: TextToDisplay -> [DisplayRow]

-- run out of text to display
getDisplayRows (TextToDisplay [] _ _ _ _ _) = []

getDisplayRows textToDisplay@(TextToDisplay text start maxWidth getCharWidth getCharBytesSize wrapMode) =
    (DisplayRow start end displayText) : getDisplayRows nextInputText

    where
        (displayText, leftOverTextEnd, end) = getDisplayRowEnd textToDisplay "" start

        (leftOverText, nextStart) =
            case wrapMode of
                Wrap -> (leftOverTextEnd, end) -- don't need to do anything for wrap mode :)

                NoWrap leftMarginOffset -> getNextStartNoWrap leftOverTextEnd end leftMarginOffset getCharBytesSize

        nextInputText = (TextToDisplay leftOverText nextStart maxWidth getCharWidth getCharBytesSize wrapMode)


-- Find the end of display row, either find a newline character or run out of space on display
getDisplayRowEnd :: TextToDisplay
                 -> String -- acummulate display text(in reverse order)
                 -> Integer -- accumulate byte position
                 -> (String, String, Integer) -- (display text, left over text, end position)

-- run out of text
getDisplayRowEnd (TextToDisplay [] _ _ _ _ _) textAcc posAcc = ((reverse textAcc), [], posAcc)

getDisplayRowEnd (TextToDisplay text@(x:xs) start maxWidth getCharWidth getCharBytesSize wrapMode) textAcc posAcc
    -- leave last character in
    | x == '\n' || maxWidth - xCharWidth ==  0 = ((reverse newTextAcc), xs, newPosAcc)

    -- overshot display width so go back a character
    | maxWidth - xCharWidth < 0 = ((reverse textAcc), text, posAcc)

    | otherwise = (getDisplayRowEnd (TextToDisplay xs start (maxWidth - xCharWidth) getCharWidth getCharBytesSize
      wrapMode) newTextAcc newPosAcc)

    where
        xCharWidth = getCharWidth x
        newTextAcc = x : textAcc
        newPosAcc = posAcc + getCharBytesSize x -- count the bytes of character


-- Find the start of the next display row when not in wrap mode
getNextStartNoWrap :: String -- left over text
             -> Integer -- byte start position
             -> Integer -- left margin offset
             -> (Char -> Integer) -- gets the number of bytes character uses
             -> (String, Integer) -- (left over text, new byte start position)

-- ran out of text
getNextStartNoWrap [] start _ _ = ([], start)


getNextStartNoWrap (x:xs) start leftMarginOffset getCharBytesSize
  | x == '\n' = getNextStartNoWrapRemoveLeftMarginOffset xs newStart leftMarginOffset getCharBytesSize

  | otherwise = getNextStartNoWrap xs newStart leftMarginOffset getCharBytesSize

  where
    charBytesLength = getCharBytesSize x
    newStart = start + charBytesLength


getNextStartNoWrapRemoveLeftMarginOffset :: String -- left over text
             -> Integer -- byte start position
             -> Integer -- left margin offset
             -> (Char -> Integer) -- gets the number of bytes character uses
             -> (String, Integer) -- (left over text, new byte start position)

getNextStartNoWrapRemoveLeftMarginOffset [] start _ _ = ([], start)

getNextStartNoWrapRemoveLeftMarginOffset leftOverText start 0 _ = (leftOverText, start)

getNextStartNoWrapRemoveLeftMarginOffset (x:xs) start leftMarginOffset getCharBytesSize =
  getNextStartNoWrapRemoveLeftMarginOffset xs newStart (leftMarginOffset - 1) getCharBytesSize
  where
    charBytesLength = getCharBytesSize x
    newStart = start + charBytesLength


