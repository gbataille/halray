module GB2.Geometry where

-- ####################################
-- Vector
-- ####################################

data Vector = Vector Float Float Float -- x, y, z
              deriving (Show, Eq)

type Normal = Vector
type Point = Vector

instance Num (Vector) where
   Vector x y z + Vector x' y' z' = Vector (x + x') (y + y') (z + z')
   Vector x y z - Vector x' y' z' = Vector (x - x') (y - y') (z - z')
   Vector x y z * Vector x' y' z' = Vector (x * x') (y * y') (z * z')
   abs (Vector x y z) = Vector (abs x) (abs y) (abs z)
   signum (Vector x y z) = Vector (signum x) (signum y) (signum z)
   fromInteger _ = error "ERROR: fromInteger :: t -> Vector is called when it should not"
   negate (Vector x y z) = Vector (-x) (-y) (-z)

norm :: Vector -> Float
norm (Vector x y z) = sqrt (x * x + y * y + z * z)

normalize :: Vector -> Vector
normalize (Vector x y z) = Vector (x / n) (y / n) (z / n) where n = norm (Vector x y z)

dot :: Vector -> Vector -> Float
dot (Vector x y z) (Vector x' y' z') = x * x' + y * y' + z * z'

cross :: Vector -> Vector-> Vector
cross (Vector x y z) (Vector x' y' z') = Vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

-- HOWTO: make vmul a cleaner operator, such as *
-- HOWTO make vmul a commutative operator without creating vmul2 ?
vmul :: Vector -> Float -> Vector
vmul (Vector x y z) s = Vector (x * s) (y * s) (z * s)

vmul2 :: Float -> Vector -> Vector
vmul2 = flip vmul

-- ####################################
-- Sphere
-- ####################################

data Primitive = Sphere { getSphereRadius :: Float, getSphereCenter :: Point } -- Radius, center
                | Triangle Point Point Point -- Three coordinates of the triangle
                deriving (Show)

type Triangle = Primitive
type Sphere = Primitive

-- ####################################
-- Space geometry
-- ####################################

{-
 Takes 2 vector leaving (or hitting) a surface from a common point and the 
 normal to the surface at that point.

 Returns True if both vectors are on the same side of the surface.

 Law: both vectors and the normal join at the same point on the surface
-}
sameSide :: Vector -- ^ outgoing vector 1
  -> Vector        -- ^ outgoing vector 2
  -> Normal        -- ^ surface's normal at the intersection point
  -> Bool
sameSide v1 v2 normal = (v1 `dot` normal) * (v2 `dot` normal) > 0

