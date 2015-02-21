module GB2.Material where

import GB2.Color
import GB2.Geometry

-- Materials
data Material = Diffuse Float Float Float | Mirror Float Float Float

materialAlbedo :: Material -> Color
materialAlbedo (Diffuse r g b) = Vector r g b
materialAlbedo (Mirror r g b) = Vector r g b

bsdf :: Material -> Color
bsdf (Diffuse r g b) = (Vector r g b) `vmul` (1/pi)
bsdf (Mirror r g b) = Vector 0 0 0

-- Later, all the different material while inherite from Material and
-- implements different sampling/evaluating methods using something
-- which may looks like virtual dispatching. I have NO idea on how this
-- will work in Haskell
