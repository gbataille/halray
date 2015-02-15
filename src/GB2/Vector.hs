module GB2.Vector where

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
