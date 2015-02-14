{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import GB2.Vector as V
import Control.Applicative

instance Arbitrary V.Vector where
    arbitrary = V.Vector <$> 
      arbitrarySizedFractional <*> arbitrarySizedFractional <*> arbitrarySizedFractional


main :: IO ()
main = htfMain htf_thisModulesTests

test_simpleAdd = assertEqual (V.vecAdd (V.Vector 1 2 3) (V.Vector 3 2 1)) (V.Vector 4.0 4.0 4.0)

prop_vecAddZero :: V.Vector -> Bool
prop_vecAddZero v = v == V.vecAdd v (V.Vector 0 0 0)
