module Core.Display (getDisplaySectionsWrap, getDisplaySectionsNoWrap) where

import Core.Types.CoreTypes

getDisplaySectionsWrap :: String -> Integer -> Integer -> (Char -> Integer) -> [DisplaySection]
getDisplaySectionsWrap [] _ _ _ = []

getDisplaySectionsWrap inputText displaySectionStart maxWidth getCharacterWidth =
    newDisplaySection : getDisplaySectionsWrap inputTextLeftAfterStartOffset displaySectionEnd maxWidth getCharacterWidth
    where
        (offset, sectionText, inputTextLeft) = calculateDisplaySectionEndOffset inputText maxWidth 0 "" getCharacterWidth
        displaySectionEnd = displaySectionStart + offset
        newDisplaySection = (DisplaySection displaySectionStart displaySectionEnd sectionText)
        -- last character left on
        inputTextLeftAfterStartOffset = tail inputTextLeft


getDisplaySectionsNoWrap :: String -> Integer -> Integer -> Integer -> (Char -> Integer) -> [DisplaySection]
getDisplaySectionsNoWrap [] _ _ _ _ = []

getDisplaySectionsNoWrap inputText displaySectionStart startOffset maxWidth getCharacterWidth =
    case displaySectionStartOffset of
        Nothing -> newDisplaySection : []
        Just (nextDisplaySectionStartOffset, inputTextLeftAfterStartOffset) -> newDisplaySection : getDisplaySectionsNoWrap
            inputTextLeftAfterStartOffset (displaySectionEnd + nextDisplaySectionStartOffset) startOffset maxWidth getCharacterWidth
    where
        (offset, sectionText, inputTextLeft) = calculateDisplaySectionEndOffset inputText maxWidth 0 "" getCharacterWidth
        displaySectionEnd = displaySectionStart + offset
        newDisplaySection = (DisplaySection displaySectionStart displaySectionEnd sectionText)
        -- last character left on inputTextLeft so move acc back a position
        accStartPosition = (- (getCharacterWidth $ head inputTextLeft))
        displaySectionStartOffset = calculateDisplaySectionStartOffsetNoWrap inputTextLeft startOffset accStartPosition getCharacterWidth


calculateDisplaySectionEndOffset :: String -> Integer -> Integer -> String -> (Char -> Integer) -> (Integer, String, String)
calculateDisplaySectionEndOffset [] _ offsetAcc sectionTextAcc _ =
    -- remember to reverse section text
    (offsetAcc, (reverse sectionTextAcc), [])

calculateDisplaySectionEndOffset inputText@(x:xs) maxWidth offsetAcc sectionTextAcc getCharacterWidth =
    if x == '\n' || maxWidth - xCharacterWidth <= 0 then
        -- remember to reverse section text
        -- leave last character in
        (newOffsetAcc, (reverse newSectionTextAcc), inputText)
    else
        -- append to section text
        calculateDisplaySectionEndOffset xs (maxWidth - xCharacterWidth) newOffsetAcc newSectionTextAcc getCharacterWidth
    where
        xCharacterWidth = getCharacterWidth x
        newSectionTextAcc = x : sectionTextAcc
        newOffsetAcc = offsetAcc + xCharacterWidth


calculateDisplaySectionStartOffsetNoWrap :: String -> Integer -> Integer -> (Char -> Integer) -> Maybe (Integer, String)
calculateDisplaySectionStartOffsetNoWrap [] _ acc _ = Nothing

calculateDisplaySectionStartOffsetNoWrap (x:xs) startOffset acc getCharacterWidth =
    if x == '\n' then
        removeStartOffset xs startOffset (acc + xCharacterWidth) getCharacterWidth
    else
        calculateDisplaySectionStartOffsetNoWrap xs startOffset (acc + xCharacterWidth) getCharacterWidth
    where
        xCharacterWidth = getCharacterWidth x


removeStartOffset :: String -> Integer -> Integer -> (Char -> Integer) -> Maybe (Integer, String)
removeStartOffset [] _ _ _ = Nothing

removeStartOffset text 0 acc _ = Just (acc, text)

removeStartOffset (x:xs) startOffset acc getCharacterWidth =
    removeStartOffset xs (startOffset - xCharacterWidth) (acc + xCharacterWidth) getCharacterWidth
    where
        xCharacterWidth = getCharacterWidth x

