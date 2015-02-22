-- | Tonemapping primitives
-- 
--   These functions handle the mapping between film measure (i.e: real flux)
--   and the screen dynamic.

module GB2.Tonemap
    (gamma22)
where

import GB2.Geometry
import GB2.Color

-- | Simple gamma correction tonemapping
gamma22 :: Color -> Color
gamma22 (Vector x y z) = Vector (g x) (g y) (g z)
  where g = (**(1.0 / 2.2))
