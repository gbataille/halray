module GB2.Geometry where

-- ####################################
-- Vector
-- ####################################

-- | 3D Vector with cartesian coordinates x y z
data Vector = Vector Float Float Float
              deriving (Show, Eq)

type Normal = Vector      -- ^ A Vector representing a surface's normal direction
type Point = Vector       -- ^ A Point in space (isomorphic to a vector)

{- |
 sum, subtraction, product are defined term by term in the Num class
-}
instance Num (Vector) where
   Vector x y z + Vector x' y' z' = Vector (x + x') (y + y') (z + z')
   Vector x y z - Vector x' y' z' = Vector (x - x') (y - y') (z - z')
   Vector x y z * Vector x' y' z' = Vector (x * x') (y * y') (z * z')
   abs (Vector x y z) = Vector (abs x) (abs y) (abs z)
   signum (Vector x y z) = Vector (signum x) (signum y) (signum z)
   fromInteger _ = error "ERROR: fromInteger :: t -> Vector is called when it should not"
   negate (Vector x y z) = Vector (-x) (-y) (-z)

-- Constants
vector0 :: Vector
vector0 = Vector 0 0 0

-- Operations
-- | The mathematical norm (≣length) of a vector
norm :: Vector -> Float
norm (Vector x y z) = sqrt (x * x + y * y + z * z)

-- | Normalize the vector (keeps its direction but has a norm of 1)
normalize :: Vector -> Vector
normalize (Vector x y z) = Vector (x / n) (y / n) (z / n) where n = norm (Vector x y z)

-- | Dot product of 2 vectors (equals to the cosine of the angle between the 2 vectors)
dot :: Vector -> Vector -> Float
dot (Vector x y z) (Vector x' y' z') = x * x' + y * y' + z * z'

-- | Cross product of 2 vectors
cross :: Vector -> Vector -> Vector
cross (Vector x y z) (Vector x' y' z') = Vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

-- | Multiplication of a vector by a scalar value, scalar second
vmul :: Vector -> Float -> Vector
vmul (Vector x y z) s = Vector (x * s) (y * s) (z * s)

-- | Multiplication of a vector by a scalar value, scalar first
vmul2 :: Float -> Vector -> Vector
vmul2 = flip vmul

-- | Return a vector pointing from the origin to the destination
vectorFromPToP :: Point       -- ^ Point of origin
               -> Point       -- ^ Point of destination
               -> Vector      -- ^ Vector linking the 2 points
vectorFromPToP origin destination = destination - origin

-- ####################################
-- Sphere
-- ####################################

-- | A Primitive is a 3D structure
data Primitive = Sphere { getSphereRadius :: Float, getSphereCenter :: Point } -- Radius, center
                | Triangle Point Point Point -- Three coordinates of the triangle
                deriving (Show)

type Triangle = Primitive
type Sphere = Primitive

-- ####################################
-- Space geometry
-- ####################################

{- |
 Takes 2 vector leaving (or hitting) a surface from a common point and the 
 normal to the surface at that point.

 Returns True if both vectors are on the same side of the surface.

 __Law:__ both vectors and the normal join at the same point on the surface
-}
sameSide :: Vector -- ^ outgoing vector 1
  -> Vector        -- ^ outgoing vector 2
  -> Normal        -- ^ surface's normal at the intersection point
  -> Bool
sameSide v1 v2 normal = (v1 `dot` normal) * (v2 `dot` normal) > 0

