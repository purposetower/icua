module Core.DisplaySuite (displaySuite) where

import Test.Tasty (testGroup, TestTree)

displaySuite :: TestTree
displaySuite = testGroup "Display" []
