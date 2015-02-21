module GB2.Material where

import GB2.Color
import GB2.Geometry

-- Materials
data Material = Diffuse Color | Mirror Color

materialAlbedo :: Material -> Color
materialAlbedo (Diffuse color) = color
materialAlbedo (Mirror color) = color

bsdf :: Material -> Color
bsdf (Diffuse color) = color `vmul` (1/pi)
bsdf (Mirror _) = Vector 0 0 0

-- Later, all the different material while inherite from Material and
-- implements different sampling/evaluating methods using something
-- which may looks like virtual dispatching. I have NO idea on how this
-- will work in Haskell
