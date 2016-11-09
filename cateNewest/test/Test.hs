import Test.Tasty

import qualified DisplayTextTest

main = defaultMain (testGroup "Tests" [DisplayTextTest.testSuite])