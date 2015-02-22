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
test_simpleAdd = assertEqual ((Vector 1 2 3) + (Vector 3 2 1)) (Vector 4.0 4.0 4.0)
test_simpleSub = assertEqual ((Vector 1 2 3) - (Vector 3 2 1)) (Vector (-2) 0 2)
test_simpleMul = assertEqual ((Vector 1 2 3) * (Vector 3 2 3)) (Vector 3 4 9)
test_abs = assertEqual (abs (Vector (-1) 2 (-3))) (Vector 1 2 3)
test_signum = assertEqual (signum (Vector (-5) 0 3)) (Vector (-1) 0 1)
test_negate = assertEqual (negate (Vector (-5) 0 3)) (Vector 5 0 (-3))

-- norm
test_norm = assertEqual (norm (Vector 4 3 0)) 5

-- normalize
test_normalize0 = assertEqual (normalize (Vector 5 0 0)) (Vector 1 0 0)
test_normalize1 = assertEqual (normalize (Vector 0 5 0)) (Vector 0 1 0)
test_normalize2 = assertEqual (normalize (Vector 0 0 5)) (Vector 0 0 1)

-- dot
test_dot = assertEqual (dot (Vector 1 2 3) (Vector 4 5 6)) 32

-- vmul
test_vmul = assertEqual ((Vector 1 2 3) `vmul` 2) (Vector 2 4 6)
test_vmul2 = assertEqual (2 `vmul2` (Vector 1 2 3)) (Vector 2 4 6)

-- cross
test_cross1 = assertEqual ((Vector 1 0 0) `cross` (Vector 0 1 0)) (Vector 0 0 1)
test_cross2 = assertEqual ((Vector 0 1 0) `cross` (Vector 0 0 1)) (Vector 1 0 0)
test_cross3 = assertEqual ((Vector 0 0 2) `cross` (Vector 1 0 0)) (Vector 0 2 0)

-- vectorFromPointToPoint
test_vectorFromPToP1 = assertEqual (vectorFromPToP (Vector 0 0 0) (Vector 1 1 1)) (Vector 1 1 1)
test_vectorFromPToP2 = assertEqual (vectorFromPToP (Vector 3 (-4) 10) (Vector 6 4 (-3))) (Vector 3 8 (-13))

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
