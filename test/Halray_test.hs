{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import GB2.Vector as V
import Control.Applicative

instance Arbitrary V.Vector where
    arbitrary = V.Vector <$> 
      arbitrarySizedFractional <*> arbitrarySizedFractional <*> arbitrarySizedFractional


main :: IO ()
main = htfMain htf_thisModulesTests

test_simpleAdd = assertEqual ((V.Vector 1 2 3) + (V.Vector 3 2 1)) (V.Vector 4.0 4.0 4.0)

prop_vecAddZero :: V.Vector -> Bool
prop_vecAddZero v = v == (v + (V.Vector 0 0 0))

test_simpleSub = assertEqual ((V.Vector 1 2 3) - (V.Vector 3 2 1)) (V.Vector (-2) 0 2)
test_simpleMul = assertEqual ((V.Vector 1 2 3) * (V.Vector 3 2 3)) (V.Vector 3 4 9)
test_abs = assertEqual (abs (V.Vector (-1) 2 (-3))) (V.Vector 1 2 3)

test_signum = assertEqual (signum (V.Vector (-5) 0 3)) (V.Vector (-1) 0 1)

test_norm = assertEqual (norm (V.Vector 4 3 0)) 5

test_normalize0 = assertEqual (normalize (V.Vector 5 0 0)) (V.Vector 1 0 0)
test_normalize1 = assertEqual (normalize (V.Vector 0 5 0)) (V.Vector 0 1 0)
test_normalize2 = assertEqual (normalize (V.Vector 0 0 5)) (V.Vector 0 0 1)

test_dot = assertEqual (dot (V.Vector 1 2 3) (V.Vector 4 5 6)) 32

-- VMUL
test_vmul = assertEqual ((V.Vector 1 2 3) `vmul` 2) (V.Vector 2 4 6)
test_vmul2 = assertEqual (2 `vmul2` (V.Vector 1 2 3)) (V.Vector 2 4 6)
