module GB2.Vector
(
  Vector(..),
  vecAdd
) where

data Vector = Vector Float Float Float 
            deriving (Eq, Show)

vecAdd :: Vector -> Vector -> Vector
vecAdd (Vector a b c) (Vector d e f) = Vector (a+d) (b+e) (c+f)
