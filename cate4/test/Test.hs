module Main (main) where

import Test.Tasty (defaultMain, testGroup, TestTree)

import Core.DisplaySuite

main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [displaySuite]
