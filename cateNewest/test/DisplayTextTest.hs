module DisplayTextTest (testSuite) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import Data.List.Split

import DisplayText

testSuite = testGroup "DisplayTextTest" [unitTests, quickCheckProperties]


unitTests = testGroup "UnitTests"
    [
    HU.testCase "Simple" $ getDisplayText "hello" (DisplaySize 10 10) Wrap False HU.@?= "hello",

    HU.testCase "Empty text" $ getDisplayText "" (DisplaySize 10 1) Wrap False HU.@?= "",

    HU.testCase "Empty display" $ getDisplayText "blabla" (DisplaySize 0 0) Wrap False HU.@?= "",
    HU.testCase "Empty display width" $ getDisplayText "aa" (DisplaySize 0 2) Wrap False HU.@?= "",
    HU.testCase "Empty display height" $ getDisplayText "aa" (DisplaySize 3 0) Wrap False HU.@?= "",

    HU.testCase "New lines" $ getDisplayText "hello\nanother\n and more" (DisplaySize 13 11) Wrap False HU.@?= "hello\nanother\n and more",

    HU.testCase "Overun width, wrap lines" $ getDisplayText "123456789" (DisplaySize 2 20) Wrap False HU.@?= "12\n34\n56\n78\n9",
    HU.testCase "Overun width, wrap lines with new lines" $ getDisplayText "123\n456789" (DisplaySize 4 20) Wrap False HU.@?= "123\n4567\n89",
    HU.testCase "Overun width with new line, wrap lines" $ getDisplayText "1\n34\n567\n89" (DisplaySize 2 20) Wrap False HU.@?= "1\n34\n56\n7\n89",

    HU.testCase "Overun height, truncate lines" $ getDisplayText "123\n456\n789" (DisplaySize 10 2) Wrap False HU.@?= "123\n456\n",
    HU.testCase "Overun width and then height" $ getDisplayText "123456789" (DisplaySize 2 4) Wrap False HU.@?= "12\n34\n56\n78",
   

    HU.testCase "Line containing only new line character" $ getDisplayText "\n" (DisplaySize 31 24) Wrap False HU.@?= "\n",
    HU.testCase "Line containing new line character" $ getDisplayText "\na\n" (DisplaySize 1999 9992) Wrap False HU.@?= "\na\n",
    HU.testCase "Line containing only new line characters" $ getDisplayText "\n\n" (DisplaySize 11 4) Wrap False HU.@?= "\n\n"
    ]


quickCheckProperties = localOption (QC.QuickCheckTests 100000) $ testGroup "QuickCheck"
    [
    QC.testProperty "Max length of output is width * height + height(incase of new lines)" maxLengthOfOutput,

    QC.testProperty "Max length of each line, shouldn't be bigger than display width" maxLengthOfLines,

    QC.testProperty "Max number of new lines should be <= display height" maxNumberOfNewLines,

    QC.testProperty "Calling getDisplayText on getDisplayText result should be idempotent" callWithResultShouldBeIdempotent
    ]


maxLengthOfOutput :: String -> QC.NonNegative Integer -> QC.NonNegative Integer -> Bool
maxLengthOfOutput input (QC.NonNegative width) (QC.NonNegative height) =
    (toInteger $ length $ getDisplayText input (DisplaySize width height) Wrap False) <= (width * height) + height


maxLengthOfLines :: String -> QC.NonNegative Integer -> QC.NonNegative Integer -> Bool
maxLengthOfLines input (QC.NonNegative width) (QC.NonNegative height) =
    null $ filter (> width) $ map (toInteger . length) splitOnNewLine
    where
        splitOnNewLine = splitOn "\n" $ getDisplayText input (DisplaySize width height) Wrap False


maxNumberOfNewLines :: String -> QC.NonNegative Integer -> QC.NonNegative Integer -> Bool
maxNumberOfNewLines input (QC.NonNegative width) (QC.NonNegative height) =
    (toInteger $ length $ filter (== '\n') (getDisplayText input (DisplaySize width height) Wrap False)) <= height


callWithResultShouldBeIdempotent :: String -> Integer -> Integer -> Bool
callWithResultShouldBeIdempotent input width height =
    getDisplayText previousCall (DisplaySize width height) Wrap False == previousCall
    where
        previousCall = getDisplayText input (DisplaySize width height) Wrap False
