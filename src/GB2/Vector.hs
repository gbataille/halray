module GB2.Vector
(
  Vector(..),
  vecAdd
) where

-- | A 3D Vector
data Vector = Vector Float Float Float 
            deriving (Eq, Show)

-- | Adds 2 3D Vectors together
vecAdd :: Vector -> Vector -> Vector
vecAdd (Vector a b c) (Vector d e f) = Vector (a+d) (b+e) (c+f)
