{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.Framework.TestManager
import Tests.GB2.Geometry

myTests :: [TestSuite]
myTests = [
          htf_thisModulesTests
          , htf_Tests_GB2_Geometry_thisModulesTests
          ]

getOneTestSuiteFromTests :: [TestSuite] -> TestSuite
getOneTestSuiteFromTests x = makeAnonTestSuite $ fmap testSuiteAsTest x

main :: IO ()
-- main = htfMain $ getOneTestSuiteFromTests myTests
main = htfMain htf_Tests_GB2_Geometry_thisModulesTests

prop_test :: Int -> Bool
prop_test i = (\_ -> True) i == True
