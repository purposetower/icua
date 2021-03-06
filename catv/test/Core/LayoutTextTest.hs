module Core.LayoutTextTest (testSuite) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import Core.Types.DisplaySize
import Core.LayoutText


testSuite = testGroup "LayoutTextTest" [unitTestsWrap, unitTestsNoWrap, quickCheckPropertiesWrap,
    quickCheckPropertiesNoWrap]


unitTestsWrap = testGroup "UnitTests Wrap"
    [
    HU.testCase "Simple" $ getLinesWrap "hello" (DisplaySize 10 2) HU.@?= ["hello"],

    -- empty
    HU.testCase "Empty input" $ getLinesWrap "" (DisplaySize 10 3) HU.@?= [],
    HU.testCase "Empty display" $ getLinesWrap "hello" (DisplaySize 0 0) HU.@?= [],
    HU.testCase "Empty display width" $ getLinesWrap "hello" (DisplaySize 0 3) HU.@?= ["", "", ""],
    HU.testCase "Empty display height" $ getLinesWrap "hello" (DisplaySize 10 0) HU.@?= [],

    -- new lines
    HU.testCase "New lines" $ getLinesWrap "12\n345\n6789" (DisplaySize 10 10) HU.@?= ["12", "345", "6789"],
    HU.testCase "Only new line" $ getLinesWrap "\n" (DisplaySize 10 10) HU.@?= [""],
    HU.testCase "Only new lines" $ getLinesWrap "\n\n" (DisplaySize 10 10) HU.@?= ["", ""],
    HU.testCase "New line sandwich" $ getLinesWrap "\na\n" (DisplaySize 10 10) HU.@?= ["", "a"],

    -- wrap
    HU.testCase "Wrap" $ getLinesWrap "hello" (DisplaySize 3 10) HU.@?= ["hel", "lo"],
    HU.testCase "Wrap multiple lines" $ getLinesWrap "12345678" (DisplaySize 2 10) HU.@?= ["12", "34", "56", "78"],
    HU.testCase "Wrap on new line" $ getLinesWrap "12\n34" (DisplaySize 2 10) HU.@?= ["12", "34"],

    -- truncate height
    HU.testCase "Truncate height" $ getLinesWrap "123456789" (DisplaySize 1 4) HU.@?= ["1", "2", "3", "4"]
    ]


unitTestsNoWrap = testGroup "UnitTests No Wrap"
    [
    HU.testCase "Simple" $ getLinesNoWrap "hello" 0 (DisplaySize 10 10) HU.@?= ["hello"],

    -- empty
    HU.testCase "Empty input" $ getLinesNoWrap "" 0 (DisplaySize 10 10) HU.@?= [],
    HU.testCase "Empty display" $ getLinesNoWrap "hello" 0 (DisplaySize 0 0) HU.@?= [],
    HU.testCase "Empty display width" $ getLinesNoWrap "hello" 0 (DisplaySize 0 2) HU.@?= ["", ""],
    HU.testCase "Empty display height" $ getLinesNoWrap "hello" 0 (DisplaySize 10 0) HU.@?= [],

    -- new lines
    HU.testCase "New lines" $ getLinesNoWrap "12\n345\n6789" 0 (DisplaySize 10 10) HU.@?= ["12", "345", "6789"],
    HU.testCase "Only new line" $ getLinesNoWrap "\n" 0 (DisplaySize 10 10) HU.@?= [""],
    HU.testCase "Only new lines" $ getLinesNoWrap "\n\n" 0 (DisplaySize 10 10) HU.@?= ["", ""],
    HU.testCase "New line sandwich" $ getLinesNoWrap "\na\n" 0 (DisplaySize 10 10) HU.@?= ["", "a"],

    -- line overflow, truncate width
    HU.testCase "Truncate width" $ getLinesNoWrap "hello" 0 (DisplaySize 3 10) HU.@?= ["hel"],
    HU.testCase "Truncate width multiple lines" $ getLinesNoWrap "hello\nhello" 0 (DisplaySize 3 10) HU.@?= ["hel", "hel"],
    HU.testCase "Truncate width on new line" $ getLinesNoWrap "12\n34" 0 (DisplaySize 2 10) HU.@?= ["12", "34"],

    -- truncate height
    HU.testCase "Truncate height" $ getLinesNoWrap "12\n3\n4\n567\n89" 0 (DisplaySize 1 4) HU.@?= ["1", "3", "4", "5"],

    -- left margin
    HU.testCase "Left margin" $ getLinesNoWrap "hello" 2 (DisplaySize 10 10) HU.@?= ["llo"],
    HU.testCase "Left margin multiple lines" $ getLinesNoWrap "hello\nhello" 2 (DisplaySize 10 10) HU.@?= ["llo", "llo"],
    HU.testCase "Left margin and truncate width" $ getLinesNoWrap "hello" 1 (DisplaySize 3 10) HU.@?= ["ell"]
    ]


quickCheckPropertiesWrap = localOption (QC.QuickCheckTests 100000) $ testGroup "QuickCheck Wrap"
    [
    QC.testProperty "Max length of each line is width" $ qcWrap maxLengthOfLines,
    QC.testProperty "Max number of lines is height" $ qcWrap maxNumberOfLines,
    QC.testProperty "Calling getLinesWrap on result is idempotent" $ qcWrap callOnResultIsIdempotent
    ]


quickCheckPropertiesNoWrap = localOption (QC.QuickCheckTests 100000) $ testGroup "QuickCheck NoWrap"
    [
    QC.testProperty "Max length of each line is width" $ qcNoWrap maxLengthOfLines,
    QC.testProperty "Max number of lines is height" $ qcNoWrap maxNumberOfLines,
    QC.testProperty "Calling getLinesNoWrap on result is idempotent" $ qcNoWrap0LeftMargin callOnResultIsIdempotent
    ]


qcWrap someFunc = someFunc getLinesWrap
qcNoWrap someFunc (QC.NonNegative leftMargin) = someFunc (\input -> getLinesNoWrap input leftMargin)
qcNoWrap0LeftMargin someFunc = someFunc (\input -> getLinesNoWrap input 0)


maxLengthOfLines :: (String -> DisplaySize -> [String]) -> String -> QC.NonNegative Integer -> QC.NonNegative Integer -> Bool
maxLengthOfLines getLinesFunc input (QC.NonNegative width) (QC.NonNegative height) =
    null $ filter (> width) $ map (toInteger . length) $ getLinesFunc input (DisplaySize width height)


maxNumberOfLines :: (String -> DisplaySize -> [String]) -> String -> QC.NonNegative Integer -> QC.NonNegative Integer -> Bool
maxNumberOfLines getLinesFunc input (QC.NonNegative width) (QC.NonNegative height) =
    (toInteger $ length $ getLinesFunc input (DisplaySize width height)) <= height


callOnResultIsIdempotent :: (String -> DisplaySize -> [String])-> String -> QC.NonNegative Integer -> QC.NonNegative Integer -> Bool
callOnResultIsIdempotent getLinesFunc input (QC.NonNegative width) (QC.NonNegative height) =
    -- ignore empty results
    if foldl (++) "" previousCall == ""
        then True
        else getLinesFunc previousCallJoined (DisplaySize width height) == previousCall
    where
        previousCall = getLinesFunc input (DisplaySize width height)
        previousCallJoined = case previousCall of
            [] -> []
            -- add extra newline sigh
            _ -> (foldl1 (\x y -> x ++ "\n" ++ y) previousCall) ++ "\n"
