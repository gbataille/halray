module GB2.Material where

import GB2.Color
import GB2.Geometry

-- Types
data Material = Diffuse Color
              | Mirror Color
              | Glass { getGlassColor :: Color, getGlassRefractionIndex :: Float }

-- Operations
materialAlbedo :: Material -> Color
materialAlbedo (Diffuse color) = color
materialAlbedo (Mirror color) = color
materialAlbedo (Glass color _) = color

bsdf :: Material -> Color
bsdf (Diffuse color) = color `vmul` (1/pi)
bsdf (Mirror _) = color0
bsdf (Glass color _) = 0.1 `vmul2` color

-- Later, all the different material while inherite from Material and
-- implements different sampling/evaluating methods using something
-- which may looks like virtual dispatching. I have NO idea on how this
-- will work in Haskell

-- | Direction of the Mirror reflected ray
reflect :: Normal -> Vector -> Vector
reflect normal c = (normal `vmul` (dot normal c) `vmul` 2) - c

-- | Direction of the refracted ray
refract :: Normal -> Vector -> Float -> Maybe Vector
refract normal i ior
  | k < 0 = Nothing
  | otherwise = Just (i' `vmul` ior' - normal' `vmul` (ior' * (dot normal' i') + sqrt(k)))
    where
     i' = -i
     (ior', normal') = if (dot normal i') > 0
                          then (ior, -normal)
                          else (1.0 / ior, normal)
     k = 1.0 - ior' * ior' * (1.0 - (dot normal' i') * (dot normal' i'))

-- | Approximation of the Fresnel factor
fresnelR :: Normal          -- ^ Normal to the surface hit
         -> Vector          -- ^ incoming vector
         -> Float           -- ^ refraction index of the "outgoing" medium (after the refracting surface)
         -> Float
fresnelR normal i ior = r0 + (1.0 - r0) * ((1.0 - (dot normal' i)) ** 5.0)
  where
    (normal', ior') = if dot normal i > 0
                         then (normal, ior)
                         else (-normal, 1.0 / ior)
    r0 = ((ior' - 1.0) / (ior' + 1.0)) ** 2.0
