{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.GB2.Primitive where

import Test.Framework
import GB2.Primitive
import GB2.Geometry

import Control.Applicative

-- #####################################
-- Vector Tests
-- #####################################

-- ##### QuickCheck #####

-- ##### HUnit #####

-- # Normals

test_normal_sphere1 = assertEqual (Vector 1 0 0) (primitiveNormal (Sphere 10 (Vector 0 0 0)) (Vector 10 0 0))
test_normal_sphere2 = assertEqual (Vector 0 0 1) (primitiveNormal (Sphere 10 (Vector 1 2 3)) (Vector 1 2 13))
test_normal_sphere3 = assertEqual (Vector 0 (-1) 0) (primitiveNormal (Sphere 10 (Vector (-7) 23 12)) (Vector (-7) 20 12))

test_normal_triangle = assertEqual (Vector 0 0 1) (primitiveNormal (Triangle (Vector 0 0 0) (Vector 1 0 0) (Vector 0 1 0)) undefined)

-- # Intersection Primitives

simpleSphere = Sphere 10 (Vector 1 0 0)

test_interect_sphere = assertEqual (Just 10) (
               rayPrimitiveIntersectDistance
                   (Ray (Vector 1 0 0) (Vector 0 0 1))
                   simpleSphere)

-- fails because intersection is < 0
test_interect_sphere_fail = assertEqual (Nothing) (
               rayPrimitiveIntersectDistance
                   (Ray (Vector 100 0 0) (Vector 0 0 1))
                   simpleSphere)

-- fails because not intersection
test_interect_sphere_fail2 = assertEqual (Nothing) (
               rayPrimitiveIntersectDistance
                   (Ray (Vector 12 0 0) (Vector 0 0 1))
                   simpleSphere)

test_intersect_triangle = assertEqual (Just 10) (
                rayPrimitiveIntersectDistance
                    (Ray (Vector 0.2 0.2 (-10)) (Vector 0 0 1))
                    (Triangle (Vector 0 0 0) (Vector 1 0 0) (Vector 0 1 0))
                )

-- fails because no intersection
test_intersect_triangle_fail = assertEqual Nothing (
                rayPrimitiveIntersectDistance
                    (Ray (Vector (-5) 0.2 (-10)) (Vector 0 0 1))
                    (Triangle (Vector 0 0 0) (Vector 1 0 0) (Vector 0 1 0))
                )

-- fails because it < 0
test_intersect_triangle_fail2 = assertEqual Nothing (
                rayPrimitiveIntersectDistance
                    (Ray (Vector 0.2 0.2 1) (Vector 0 0 1))
                    (Triangle (Vector 0 0 0) (Vector 1 0 0) (Vector 0 1 0))
                )
