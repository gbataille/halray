module GB2.Material where

import GB2.Color
import GB2.Geometry

-- Types
data Material = Diffuse Color | Mirror Color | Glass Color

-- Operations
materialAlbedo :: Material -> Color
materialAlbedo (Diffuse color) = color
materialAlbedo (Mirror color) = color
materialAlbedo (Glass color) = color

bsdf :: Material -> Color
bsdf (Diffuse color) = color `vmul` (1/pi)
bsdf (Mirror _) = color0
bsdf (Glass color) = color0 --0.5 `vmul2` color

-- Later, all the different material while inherite from Material and
-- implements different sampling/evaluating methods using something
-- which may looks like virtual dispatching. I have NO idea on how this
-- will work in Haskell
