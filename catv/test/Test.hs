import Test.Tasty

import qualified LayoutTextTest

main = defaultMain (testGroup "Tests" [LayoutTextTest.testSuite])