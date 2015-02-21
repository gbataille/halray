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

-- Objects
-- An object is a primitive with a material
data Object = Object { getObjectPrimitive :: Primitive, getObjectMaterial :: Material }

-- Scene
type Scene = [Object]
data It = It {
        getItPoint :: Point,            -- ^ Intersection point
        getItDirToRayOrig :: Vector,    -- ^ Normalized vector pointing to the incoming ray origin
        getItObject :: Object,          -- ^ Object intersected
        getItNormal :: Vector           -- ^ Normal to the surface at the intersection point
        } -- Float is the t of intersection (distance between ray origin and light)
data Light = Light { getLightPosition :: Point, getLightColor :: Color }

primitiveNormal :: Primitive    -- ^ The surface under study
  -> Point                      -- ^ The point on which the normal is to be computed
  -> Normal

primitiveNormal (Sphere _ center) point = normalize (point - center)
primitiveNormal (Triangle p0 p1 p2) _ = normalize $ cross (p1 - p0) (p2 - p0)

{-|
Computes the intersection between a ray and a sphere
The ray direction MUST be normalized

The maths are trivials. the ray coordinates are
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
rayPrimitiveIntersectDistance :: Ray    -- ^ a Ray with a normalized direction
  -> Primitive                          -- ^ the Primitive we are trying to intersect the ray with
  -> Maybe Float                        -- ^ the distance between the ray origin and the intersection point

rayPrimitiveIntersectDistance (Ray origin direction) (Sphere radius center)
  | det_o_4 < 0 = Nothing
  | t0 >= 0 = Just t0
  | t1 >= 0 = Just t1
  | otherwise = Nothing
  where
    op = center - origin
    b_o_2 = dot op direction
    det_o_4 = (b_o_2 * b_o_2) - (dot op op) + (radius * radius)
    t0 = b_o_2 - sqrt det_o_4
    t1 = b_o_2 + sqrt det_o_4

rayPrimitiveIntersectDistance (Ray origin direction) (Triangle v0 v1 v2)
  | abs a < 0.00001 = Nothing
  | u < 0 || u > 1 = Nothing
  | v < 0 || (u + v) > 1 = Nothing
  | t < 0 = Nothing
  | otherwise = Just t
  where
    e1 = v1 - v0
    e2 = v2 - v0

    h = cross direction e2
    a = dot e1 h

    f = 1 / a
    s = origin - v0

    u = f * dot s h

    q = cross s e1
    v = f * dot direction q

    t = f * dot e2 q

-- | Finds the closest intersection
-- TODO: understand how I can make something cleaner than that !
intersectScene :: Scene 
  -> Ray 
  -> Maybe (Float, It)      -- ^ Returns the distance to the object and the intersection with the object
intersectScene [] _ = Nothing
intersectScene (object@(Object prim _):trail) ray =
                case intersectScene trail ray of
                   Nothing -> case rayPrimitiveIntersectDistance ray prim of
                      Nothing -> Nothing
                      Just t -> Just (t, It point direction object normal)
                        where
                          direction = - (getRayDirection ray)
                          point = (getRayOrigin ray) + (t `vmul2` (getRayDirection ray))
                          normal = primitiveNormal prim point
                   Just (t, closestSphereIntersection) -> case rayPrimitiveIntersectDistance ray prim of
                          Nothing -> Just (t, closestSphereIntersection)
                          Just t2 -> if t < t2
                                        then Just (t, closestSphereIntersection)
                                        else Just (t2, It point direction object normal)
                                          where
                                            direction = - (getRayDirection ray)
                                            point = (getRayOrigin ray) + (t2 `vmul2` (getRayDirection ray))
                                            normal = primitiveNormal prim point
