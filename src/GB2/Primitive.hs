module GB2.Primitive where

-- In a near future, A scene may become a ray intersection
-- accelerator, and Object may be something else than primitives (such as
-- transform or aggregate of primitives)

import GB2.Geometry
import GB2.Material
import GB2.Color

-- Rays
data Ray = Ray { getRayOrigin :: Vector, getRayDirection :: Vector } -- Origin / Direction
-- TODO: check how the hell I can be explicit and object field name

-- Primitives
type Primitive = Sphere
-- TODO: latter, set triangles, aabb, ...

-- Objects
-- An object is a primitive with a material
data Object = Object { getObjectPrimitive :: Primitive, getObjectMaterial :: Material }

-- Scene
type Scene = [Object]
type It = (Float, Object) -- Float is the t of intersection (distance between ray origin and light)
data Light = Light { getLightPosition :: Point, getLightColor :: Color }

{-|
Computes the intersection between a ray and a sphere
The ray direction MUST be normalized

The maths are trivials. the ray coordinate is
P = Origin + t * Direction
An element on the sphere is
|| Center - P || == radius
(|| is the norm L2)

Then,
|| Center - Origin + t * Direction || == radius
|| Center - Origin + t * Direction || ^ 2 == radius ^ 2
|| Center - Origin || ^ 2 + t ^ 2 || Direction || ^ 2 + 2 * t * { Direction, Center - Origin } == radius ^ 2
(with { a, b } the dot product)

Because Direction is normalized, we get || Direction || == 1
Hence
t ^ 2 + t * 2 { Direction, Center - Origin } + ||Center - Origin|| ^ 2 - radius ^ 2 == 0

Second order equation, we get
a = 1
b = 2 { Direction, Center - Origin }
c = ||Origin|| ^ 2 - radius ^ 2

det = b ^ 2 - 4 (a * c)
= 4 { Direction, Center -Origin } ^ 2 - 4 (|| Center - Origin || ^ 2 - radius ^ 2)

We can simplify det by 4, just for the sake of it ;)
det_o_4 = { Direction, Center -Origin } ^ 2 - || Center - Origin || ^ 2 + radius ^ 2

if det < 0 (or det_o_4 < 0) -> No solution
if det > 0, two solutions :
t0 = (-b - sqrt(det)) / (2 * a)

(Observe that there is a 2 in b, which cancels with the 2 in (2a) and the 4 of det cancels too, so
t0 = b_o_2 - sqrt(det_o_4)

With a similar method, we get
t1 = b_o_2 + sqrt(det_o_4)

-}
raySphereIntersect :: Ray       -- ^ a Ray with a normalized direction
                -> Sphere       -- ^ the Sphere we are trying to intersect the ray with
                -> Maybe Float  -- ^ the distance between the ray origin and the intersection point
raySphereIntersect (Ray origin direction) (radius, center) =
                      if det_o_4 < 0
                         then Nothing
                         else
                           if t0 >= 0
                           then Just t0
                           else if t1 >= 0
                                   then Just t1
                                   else Nothing
                      where
                            op = center - origin
                            b_o_2 = dot op direction
                            det_o_4 = (b_o_2 * b_o_2) - (dot op op) + (radius * radius)
                            t0 = b_o_2 - sqrt det_o_4
                            t1 = b_o_2 + sqrt det_o_4



-- Find the closest intersection
-- TODO: understand how I can make something cleaner than that !
intersectScene :: Scene -> Ray -> Maybe It
intersectScene [] _ = Nothing
intersectScene (object@(Object prim material):trail) ray =
                case intersectScene trail ray of
                   Nothing -> case raySphereIntersect ray prim of
                      Nothing -> Nothing
                      Just t -> Just (t, object)
                   Just (t, closestsphere) -> case raySphereIntersect ray prim of
                          Nothing -> Just (t, closestsphere)
                          Just t2 -> if t < t2
                                        then Just (t, closestsphere)
                                        else Just (t2, object)
