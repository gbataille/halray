{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.Framework.TestManager
import Tests.GB2.Geometry

main :: IO ()
main = htfMain allTests
  where allTests = addToTestSuite htf_thisModulesTests $ [testSuiteAsTest htf_Tests_GB2_Geometry_thisModulesTests]

prop_test :: Int -> Bool
prop_test i = (\_ -> True) i == True
