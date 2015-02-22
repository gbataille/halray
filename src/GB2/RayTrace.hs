module GB2.RayTrace where

import GB2.Primitive
import GB2.Geometry
import GB2.Color
import GB2.Material

-- | Returns the energy transmitted by a given light at an intersection point
directLighting :: Scene -> It -> Light -> Color
directLighting scene it light
  | vectorsOnSameSideOfTheSurface && not (hasOcclusion scene lightRay d) =
    ((bsdf material) * (getLightColor light) `vmul` (1.0 / d_square )`vmul` (abs (dot normalAtIntersect wo)))
  | otherwise = color0
    where
      lightP = getLightPosition light
      lightDir = vectorFromPToP lightP intersectP

      d_square = normSquared lightDir
      d = sqrt d_square

      lightRay = Ray lightP (normalize lightDir)

      material = (getObjectMaterial $ getItObject it)
      intersectP = (getItPoint it)
      incomingRay = (getItDirToRayOrig it)
      normalAtIntersect = (getItNormal it)

      wo = -(normalize lightDir)

      vectorsOnSameSideOfTheSurface = sameSide incomingRay wo normalAtIntersect

-- The raytrace function
-- Display the color of the sphere hit by the ray
radianceRay :: Scene -> Light -> Ray -> Color
radianceRay scene light ray = case intersectScene scene ray of
  Just (_, intersect) -> directLighting scene intersect light
  Nothing -> color0

radianceXY :: Scene          -- ^ Scene to render
           -> Light          -- ^ Light
           -> Int            -- ^ number of samples
           -> (Float, Float) -- ^ (x, y)
           -> Color
radianceXY scene light spp coord =
  (vmul2 (1.0 / (fromIntegral spp))) $ foldl (+) color0 [ radianceRay scene light (getCameraRay sample coord) | sample <- [0..(spp-1)] ]

-- Camera bullshit, we will need to improve this
near :: Float
near = 140

far :: Float
far = 10000.0

fov :: Float
fov = 40

{-
 The idea is simple, but complexe to apply.
 We have two planes, on called "Near" at z = near and far at z = far.
 First plane is between 0 and 100 for x and y
 second plane is between 0 and 100 times fov for x and y.

 If fov is 1, all rays are parallel
 if fov is > 1, rays diverges and gives the perspective effect ;)

 Generate camera ray
 (x and y are in an unit cube of [-1, 2] ^ 2
 sample is not used yet, it is for super sampling of the pixel
-}
getCameraRay :: Int              -- ^ Sample ID
             -> (Float, Float)   -- ^ (x, y) coordinates
             -> Ray
getCameraRay _ (x, y) = (Ray p0 direction)
  where
    (fx, fy) = (x, -y)
    -- p0, on near is in [0, 100] ^ 2 at z = 140
    p0 = Vector ((fx + 1.0) * 50.0) ((fy + 1.0) * 50.0) near
    -- p1, on far is at [0, 100] ^ 2 scalled by fov and at z = -far
    -- yes, depth is inverted
    p1 = Vector ((fx * fov + 1.0) * 50.0) ((fy * fov + 1.0) * 50.0) (-far)
    direction = normalize (p1 - p0)


coordListPlane :: Int       -- ^ width
               -> Int       -- ^ height
               -> [(Float, Float)]
coordListPlane w h = do
  y <- [0..(h-1)]
  x <- [0..(w-1)]
  let xImgPlane = (2.0 * (fromIntegral x) / (fromIntegral w) - 1.0)
      yImpPlane = (2.0 * (fromIntegral y) / (fromIntegral h) - 1.0)
  return (xImgPlane, yImpPlane)

-- | Render an image of size Width x Height, with Sample super sample, with a scene description and a light
render :: Int       -- ^ width
       -> Int       -- ^ height
       -> Int       -- ^ spp
       -> Scene     -- ^ Scene to render
       -> Light     -- ^ Light in the scene
       -> [Color]
render width height spp scene light =
    (fmap
      (radianceXY scene light spp)
      (coordListPlane width height)
    )
