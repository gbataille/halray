module Material
(
    Material, -- Generic material
    Diffuse -- Diffuse material
)
where

import Color

-- Materials
type Diffuse = Color
type Material = Diffuse

-- Later, all the different material while inherite from Material and
-- implements different sampling/evaluating methods using something
-- which may looks like virtual dispatching. I have NO idea on how this
-- will work in Haskell
