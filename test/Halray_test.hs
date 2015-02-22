{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.Framework.TestManager
import Tests.GB2.Geometry
import Tests.GB2.Primitive

myTests :: [TestSuite]
myTests = [
          htf_thisModulesTests
          , htf_Tests_GB2_Geometry_thisModulesTests
          , htf_Tests_GB2_Primitive_thisModulesTests
          ]

getOneTestSuiteFromTests :: [TestSuite] -> TestSuite
getOneTestSuiteFromTests x = makeAnonTestSuite $ fmap testSuiteAsTest x

main :: IO ()
-- main = htfMain $ getOneTestSuiteFromTests myTests
main = htfMain myTests

prop_test :: Int -> Bool
prop_test i = (\_ -> True) i == True
