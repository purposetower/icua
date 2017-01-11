import Test.Tasty

import qualified Core.LayoutTextTest

main = defaultMain (testGroup "Tests" [Core.LayoutTextTest.testSuite])