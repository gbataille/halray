{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.GB2.Geometry where

import Test.Framework
import GB2.Geometry
import Control.Applicative

-- #####################################
-- Vector Tests
-- #####################################

-- ##### QuickCheck #####
instance Arbitrary Vector where
    arbitrary = Vector <$>
      arbitrarySizedFractional <*> arbitrarySizedFractional <*> arbitrarySizedFractional

prop_vecAddZero :: Vector -> Bool
prop_vecAddZero v = v == (v + (Vector 0 0 0))

prop_vecBetween2Points :: Point -> Point -> Bool
prop_vecBetween2Points p1@(Vector a b c) p2@(Vector a' b' c') =
    (vectorFromPToP p1 p2) == Vector (a' - a) (b' - b) (c' - c)

-- ##### HUnit #####
-- Num instance
test_simpleAdd = assertEqual (Vector 4.0 4.0 4.0) ((Vector 1 2 3) + (Vector 3 2 1))
test_simpleSub = assertEqual (Vector (-2) 0 2) ((Vector 1 2 3) - (Vector 3 2 1))
test_simpleMul = assertEqual (Vector 3 4 9) ((Vector 1 2 3) * (Vector 3 2 3))
test_abs = assertEqual (Vector 1 2 3) (abs (Vector (-1) 2 (-3)))
test_signum = assertEqual (Vector (-1) 0 1) (signum (Vector (-5) 0 3))
test_negate = assertEqual (Vector 5 0 (-3)) (negate (Vector (-5) 0 3))

-- norm
test_norm = assertEqual 5 (norm (Vector 4 3 0))
test_normSquared = assertEqual 29 (normSquared (Vector (-4) 3 2))

-- normalize
test_normalize0 = assertEqual (Vector 1 0 0) (normalize (Vector 5 0 0))
test_normalize1 = assertEqual (Vector 0 1 0) (normalize (Vector 0 5 0))
test_normalize2 = assertEqual (Vector 0 0 1) (normalize (Vector 0 0 5))

-- dot
test_dot = assertEqual 32 (dot (Vector 1 2 3) (Vector 4 5 6))

-- vmul
test_vmul = assertEqual (Vector 2 4 6) ((Vector 1 2 3) `vmul` 2)
test_vmul2 = assertEqual (Vector 2 4 6) (2 `vmul2` (Vector 1 2 3))

-- cross
test_cross1 = assertEqual (Vector 0 0 1) ((Vector 1 0 0) `cross` (Vector 0 1 0))
test_cross2 = assertEqual (Vector 1 0 0) ((Vector 0 1 0) `cross` (Vector 0 0 1))
test_cross3 = assertEqual (Vector 0 2 0) ((Vector 0 0 2) `cross` (Vector 1 0 0))

-- vectorFromPointToPoint
test_vectorFromPToP1 = assertEqual (Vector 1 1 1) (vectorFromPToP (Vector 0 0 0) (Vector 1 1 1))
test_vectorFromPToP2 = assertEqual (Vector 3 8 (-13)) (vectorFromPToP (Vector 3 (-4) 10) (Vector 6 4 (-3)))

-- #####################################
-- Sphere Tests
-- #####################################

-- #####################################
-- Space geometry Tests
-- #####################################
-- sameSide
test_sameSide1 = assertBool (sameSide (Vector 1 1 0) (Vector (-1) 1 0) (Vector 0 1 0))
test_sameSide2 = assertBool $ not (sameSide (Vector 1 1 0) (Vector (-1) (-1) 0) (Vector 0 1 0))
test_sameSide3 = assertBool (sameSide (Vector 0 1 0) (Vector 0 1 0) (Vector 0 1 0))

-- Vectors orthogonal to the normal are not on the same side by convention
test_sameSide_orth = assertBool $ not (sameSide (Vector 0 0 1) (Vector 1 0 0) (Vector 0 1 0))
