module LayoutTextTest (testSuite) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import Data.List.Split (splitOn)

import LayoutText

testSuite = testGroup "DisplayTextTest" [unitTestsWrapNoPad, unitTestsWrapPad, unitTestsNoWrapNoPad,
    unitTestsNoWrapPad, quickCheckPropertiesWrapNoPad, quickCheckPropertiesWrapPad,
    quickCheckPropertiesNoWrapNoPad, quickCheckPropertiesNoWrapPad]

unitTestsWrapNoPad = testGroup "UnitTests Wrap and No Pad"
    [
    HU.testCase "Simple" $ layoutText "hello" (DisplaySize 10 10) Wrap NoPad HU.@?= "hello",

    -- empty
    HU.testCase "Empty input" $ layoutText "" (DisplaySize 10 10) Wrap NoPad HU.@?= "",
    HU.testCase "Empty display" $ layoutText "hello" (DisplaySize 0 0) Wrap NoPad HU.@?= "",
    HU.testCase "Empty display width" $ layoutText "hello" (DisplaySize 0 10) Wrap NoPad HU.@?= "",
    HU.testCase "Empty display height" $ layoutText "hello" (DisplaySize 10 0) Wrap NoPad HU.@?= "",

    -- new lines
    HU.testCase "New lines" $ layoutText "12\n345\n6789" (DisplaySize 10 10) Wrap NoPad HU.@?= "12\n345\n6789",
    HU.testCase "Only new line" $ layoutText "\n" (DisplaySize 10 10) Wrap NoPad HU.@?= "\n",
    HU.testCase "Only new lines" $ layoutText "\n\n" (DisplaySize 10 10) Wrap NoPad HU.@?= "\n\n",
    HU.testCase "New line sandwich" $ layoutText "\na\n" (DisplaySize 10 10) Wrap NoPad HU.@?= "\na\n",

    -- wrap
    HU.testCase "Wrap" $ layoutText "hello" (DisplaySize 3 10) Wrap NoPad HU.@?= "hel\nlo",
    HU.testCase "Wrap multiple lines" $ layoutText "12345678" (DisplaySize 2 10) Wrap NoPad HU.@?= "12\n34\n56\n78",
    HU.testCase "Wrap on new line" $ layoutText "12\n34" (DisplaySize 2 10) Wrap NoPad HU.@?= "12\n34",

    -- truncate height
    HU.testCase "Truncate height" $ layoutText "123456789" (DisplaySize 1 4) Wrap NoPad HU.@?= "1\n2\n3\n4"
    ]

unitTestsWrapPad = testGroup "UnitTests Wrap and Pad"
    [
    HU.testCase "Simple" $ layoutText "hello" (DisplaySize 6 3) Wrap Pad HU.@?= "hello \n      \n      ",

    -- empty
    HU.testCase "Empty input" $ layoutText "" (DisplaySize 2 2) Wrap Pad HU.@?= "  \n  ",
    HU.testCase "Empty display" $ layoutText "hello" (DisplaySize 0 0) Wrap Pad HU.@?= "",
    HU.testCase "Empty display width" $ layoutText "hello" (DisplaySize 0 10) Wrap Pad HU.@?= "",
    HU.testCase "Empty display height" $ layoutText "hello" (DisplaySize 10 0) Wrap Pad HU.@?= "",

    -- new lines
    HU.testCase "New lines" $ layoutText "12\n345\n6789" (DisplaySize 5 3) Wrap Pad HU.@?= "12   \n345  \n6789 ",
    HU.testCase "Only new line" $ layoutText "\n" (DisplaySize 4 2) Wrap Pad HU.@?= "    \n    ",
    HU.testCase "Only new lines" $ layoutText "\n\n" (DisplaySize 2 3) Wrap Pad HU.@?= "  \n  \n  ",
    HU.testCase "New line sandwich" $ layoutText "\na\n" (DisplaySize 2 3) Wrap Pad HU.@?= "  \na \n  ",

    -- wrap
    HU.testCase "Wrap" $ layoutText "hello" (DisplaySize 3 3) Wrap Pad HU.@?= "hel\nlo \n   ",
    HU.testCase "Wrap multiple lines" $ layoutText "1234567" (DisplaySize 2 5) Wrap Pad HU.@?= "12\n34\n56\n7 \n  ",
    HU.testCase "Wrap on new line" $ layoutText "12\n34" (DisplaySize 2 2) Wrap Pad HU.@?= "12\n34",

    -- truncate height
    HU.testCase "Truncate height" $ layoutText "1\n23456789" (DisplaySize 2 4) Wrap Pad HU.@?= "1 \n23\n45\n67",

    -- remove last new line so we don't overflow display
    HU.testCase "Remove last new line" $ layoutText "hello" (DisplaySize 6 2) Wrap Pad HU.@?= "hello \n      ",
    HU.testCase "Remove last new line!" $ layoutText "\n" (DisplaySize 3 1) Wrap Pad HU.@?= "   "
    ]

unitTestsNoWrapNoPad = testGroup "UnitTests No Wrap and No Pad"
    [
    HU.testCase "Simple" $ layoutText "hello" (DisplaySize 10 10) (NoWrap 0) NoPad HU.@?= "hello",

    -- empty
    HU.testCase "Empty input" $ layoutText "" (DisplaySize 10 10) (NoWrap 0) NoPad HU.@?= "",
    HU.testCase "Empty display" $ layoutText "hello" (DisplaySize 0 0) (NoWrap 0) NoPad HU.@?= "",
    HU.testCase "Empty display width" $ layoutText "hello" (DisplaySize 0 10) (NoWrap 0) NoPad HU.@?= "",
    HU.testCase "Empty display height" $ layoutText "hello" (DisplaySize 10 0) (NoWrap 0) NoPad HU.@?= "",

    -- new lines
    HU.testCase "New lines" $ layoutText "12\n345\n6789" (DisplaySize 10 10) (NoWrap 0) NoPad HU.@?= "12\n345\n6789",
    HU.testCase "Only new line" $ layoutText "\n" (DisplaySize 10 10) (NoWrap 0) NoPad HU.@?= "\n",
    HU.testCase "Only new lines" $ layoutText "\n\n" (DisplaySize 10 10) (NoWrap 0) NoPad HU.@?= "\n\n",
    HU.testCase "New line sandwich" $ layoutText "\na\n" (DisplaySize 10 10) (NoWrap 0) NoPad HU.@?= "\na\n",

    -- line overflow, truncate width
    HU.testCase "Truncate width" $ layoutText "hello" (DisplaySize 3 10) (NoWrap 0) NoPad HU.@?= "hel",
    HU.testCase "Truncate width multiple lines" $ layoutText "hello\nhello" (DisplaySize 3 10) (NoWrap 0) NoPad HU.@?= "hel\nhel",
    HU.testCase "Truncate width on new line" $ layoutText "12\n34" (DisplaySize 2 10) (NoWrap 0) NoPad HU.@?= "12\n34",

    -- truncate height
    HU.testCase "Truncate height" $ layoutText "12\n3\n4\n567\n89" (DisplaySize 1 4) (NoWrap 0) NoPad HU.@?= "1\n3\n4\n5",

    -- left margin
    HU.testCase "Left margin" $ layoutText "hello" (DisplaySize 10 10) (NoWrap 2) NoPad HU.@?= "llo",
    HU.testCase "Left margin multiple lines" $ layoutText "hello\nhello" (DisplaySize 10 10) (NoWrap 2) NoPad HU.@?= "llo\nllo",
    HU.testCase "Left margin and truncate width" $ layoutText "hello" (DisplaySize 3 10) (NoWrap 1) NoPad HU.@?= "ell"
    ]

unitTestsNoWrapPad = testGroup "UnitTests No Wrap and Pad"
    [
    HU.testCase "Simple" $ layoutText "hello" (DisplaySize 6 3) (NoWrap 0) Pad HU.@?= "hello \n      \n      ",

    -- empty
    HU.testCase "Empty input" $ layoutText "" (DisplaySize 2 2) (NoWrap 0) Pad HU.@?= "  \n  ",
    HU.testCase "Empty display" $ layoutText "hello" (DisplaySize 0 0) (NoWrap 0) Pad HU.@?= "",
    HU.testCase "Empty display width" $ layoutText "hello" (DisplaySize 0 10) (NoWrap 0) Pad HU.@?= "",
    HU.testCase "Empty display height" $ layoutText "hello" (DisplaySize 10 0) (NoWrap 0) Pad HU.@?= "",

    -- new lines
    HU.testCase "New lines" $ layoutText "12\n345\n6789" (DisplaySize 5 3) (NoWrap 0) Pad HU.@?= "12   \n345  \n6789 ",
    HU.testCase "Only new line" $ layoutText "\n" (DisplaySize 4 2) (NoWrap 0) Pad HU.@?= "    \n    ",
    HU.testCase "Only new lines" $ layoutText "\n\n" (DisplaySize 2 3) (NoWrap 0) Pad HU.@?= "  \n  \n  ",
    HU.testCase "New line sandwich" $ layoutText "\na\n" (DisplaySize 2 3) (NoWrap 0) Pad HU.@?= "  \na \n  ",

    -- line overflow, truncate width
    HU.testCase "Truncate width" $ layoutText "hello" (DisplaySize 3 3) (NoWrap 0) Pad HU.@?= "hel\n   \n   ",
    HU.testCase "Truncate width multiple lines" $ layoutText "hello\nhello" (DisplaySize 3 3) (NoWrap 0) Pad HU.@?= "hel\nhel\n   ",
    HU.testCase "Truncate width on new line" $ layoutText "12\n34" (DisplaySize 2 2) (NoWrap 0) Pad HU.@?= "12\n34",

    -- truncate height
    HU.testCase "Truncate height" $ layoutText "1\n23456789" (DisplaySize 2 4) (NoWrap 0) Pad HU.@?= "1 \n23\n  \n  ",

    -- left margin
    HU.testCase "Left margin" $ layoutText "hello" (DisplaySize 5 1) (NoWrap 2) Pad HU.@?= "llo  ",
    HU.testCase "Left margin multiple lines" $ layoutText "hello\nhello" (DisplaySize 4 3) (NoWrap 2) Pad HU.@?= "llo \nllo \n    ",
    HU.testCase "Left margin and truncate width" $ layoutText "hello" (DisplaySize 3 2) (NoWrap 1) Pad HU.@?= "ell\n   ",

    -- remove last new line so we don't overflow display
    HU.testCase "Remove last new line" $ layoutText "hello" (DisplaySize 6 2) (NoWrap 0) Pad HU.@?= "hello \n      ",
    HU.testCase "Remove last new line!" $ layoutText "\n" (DisplaySize 3 1) (NoWrap 0) Pad HU.@?= "   "
    ]

quickCheckPropertiesWrapNoPad = localOption (QC.QuickCheckTests 100000) $ testGroup "QuickCheck Wrap and No Pad"
    [
    QC.testProperty "Max length of output is (width + 1(for each new line)) * height" $ maxLengthOfOutput Wrap,
    QC.testProperty "Max length of each line is width" $ maxLengthOfLines Wrap,
    QC.testProperty "Max number of new lines is less than or equal to height" $ maxNumberOfNewLines Wrap,
    QC.testProperty "Calling layoutText on layoutTextResult is idempotent" $ callOnResultIsIdempotent Wrap NoPad
    ]

quickCheckPropertiesWrapPad = localOption (QC.QuickCheckTests 100000) $ testGroup "QuickCheck Wrap and Pad"
    [
    QC.testProperty "Length of output is ((width + 1(for each new line)) * height) - 1(remove last new line)" $ lengthOfOutput Wrap,
    QC.testProperty "Length of each line is width" $ lengthOfLines Wrap,
    QC.testProperty "Number of new lines is height - 1" $ numberOfNewLines Wrap,
    QC.testProperty "Calling layoutText on layoutTextResult is idempotent" $ callOnResultIsIdempotent Wrap Pad
    ]

quickCheckPropertiesNoWrapNoPad = localOption (QC.QuickCheckTests 100000) $ testGroup "QuickCheck No Wrap and No Pad"
    [
    QC.testProperty "Max length of output is (width + 1(for each new line)) * height" $ qcNoWrap maxLengthOfOutput,
    QC.testProperty "Max length of each line is width" $ qcNoWrap maxLengthOfLines,
    QC.testProperty "Max number of new lines is less than or equal to height" $ qcNoWrap maxNumberOfNewLines,
    QC.testProperty "Calling layoutText on layoutTextResult is idempotent" $ callOnResultIsIdempotent (NoWrap 0) NoPad
    ]

quickCheckPropertiesNoWrapPad = localOption (QC.QuickCheckTests 100000) $ testGroup "QuickCheck No Wrap and Pad"
    [
    QC.testProperty "Length of output is ((width + 1(for each new line)) * height) - 1(remove last new line)" $ qcNoWrap lengthOfOutput,
    QC.testProperty "Length of each line is width" $ qcNoWrap lengthOfLines,
    QC.testProperty "Number of new lines is height - 1" $ qcNoWrap numberOfNewLines,
    QC.testProperty "Calling layoutText on layoutTextResult is idempotent" $ callOnResultIsIdempotent (NoWrap 0) Pad
    ]

maxLengthOfOutput :: WrapMode -> String -> QC.NonNegative Integer -> QC.NonNegative Integer -> Bool
maxLengthOfOutput wrapMode input (QC.NonNegative width) (QC.NonNegative height) =
    (toInteger $ length $ layoutText input (DisplaySize width height) wrapMode NoPad) <= (width + 1) * height

lengthOfOutput :: WrapMode -> String -> QC.Positive Integer -> QC.Positive Integer -> Bool
lengthOfOutput wrapMode input (QC.Positive width) (QC.Positive height) =
    (toInteger $ length $ layoutText input (DisplaySize width height) wrapMode Pad) == ((width + 1) * height) - 1

maxLengthOfLines :: WrapMode -> String -> QC.NonNegative Integer -> QC.NonNegative Integer -> Bool
maxLengthOfLines wrapMode input (QC.NonNegative width) (QC.NonNegative height) =
    null $ filter (> width) $ map (toInteger . length) splitOnNewLine
    where
        splitOnNewLine = splitOn "\n" $ layoutText input (DisplaySize width height) wrapMode NoPad

lengthOfLines :: WrapMode -> String -> QC.Positive Integer -> QC.Positive Integer -> Bool
lengthOfLines wrapMode input (QC.Positive width) (QC.Positive height) =
    null $ filter (/= width) $ map (toInteger . length) splitOnNewLine
    where
        splitOnNewLine = splitOn "\n" $ layoutText input (DisplaySize width height) wrapMode Pad

maxNumberOfNewLines :: WrapMode -> String -> QC.NonNegative Integer -> QC.NonNegative Integer -> Bool
maxNumberOfNewLines wrapMode input (QC.NonNegative width) (QC.NonNegative height) =
    (toInteger $ length $ filter (== '\n') (layoutText input (DisplaySize width height) wrapMode NoPad)) <= height

numberOfNewLines :: WrapMode -> String -> QC.Positive Integer -> QC.Positive Integer -> Bool
numberOfNewLines wrapMode input (QC.Positive width) (QC.Positive height) =
    (toInteger $ length $ filter (== '\n') (layoutText input (DisplaySize width height) wrapMode Pad)) == height - 1

callOnResultIsIdempotent :: WrapMode -> PadMode -> String -> Integer -> Integer -> Bool
callOnResultIsIdempotent wrapMode padMode input width height =
    layoutText previousCall (DisplaySize width height) wrapMode padMode == previousCall
    where
        previousCall = layoutText input (DisplaySize width height) wrapMode padMode

qcNoWrap :: (WrapMode -> a) -> QC.NonNegative Integer -> a
qcNoWrap someFunc (QC.NonNegative leftMargin) = someFunc (NoWrap leftMargin)
