module GB2.RayTrace where

import GB2.Primitive
import GB2.Geometry
import GB2.Color
import GB2.Material

import Data.Maybe (isJust, fromJust)

epsilon :: Float
epsilon = 1

-- | Returns the energy transmitted by a given light at an intersection point
intersectToEnergy :: It -> Light -> Color
intersectToEnergy it light
  | vectorsOnSameSideOfTheSurface =
    ((bsdf material) * (getLightColor light) `vmul` (1.0/(d**2) )`vmul` (abs (dot normalAtIntersect dirRay)))
  | otherwise = color0
    where
      material = (getObjectMaterial $ getItObject it)
      intersectP = (getItPoint it)
      incomingRay = (getItDirToRayOrig it)
      dirRay = normalize (vectorFromPToP intersectP (getLightPosition light))
      normalAtIntersect = (getItNormal it)
      d = norm (vectorFromPToP (getLightPosition light) intersectP)

      vectorsOnSameSideOfTheSurface = sameSide incomingRay dirRay normalAtIntersect

-- The raytrace function
-- Display the color of the sphere hit by the ray
radianceRay :: Scene -> Light -> Ray -> Color
radianceRay scene light ray
  | intersectExists && (not maskingObject) = intersectToEnergy intersect light
  | otherwise = color0
    where
     mIntersectDetails = intersectScene scene ray
     intersectExists = isJust mIntersectDetails
     (_, intersect) = fromJust mIntersectDetails

     intersectP = (getItPoint intersect)
     d = norm (vectorFromPToP (getLightPosition light) intersectP)
     lightRay = Ray (getLightPosition light) (normalize (
                    vectorFromPToP (getLightPosition light) (getItPoint intersect)
                    ))
     mLightIntersect = intersectScene scene lightRay
     (dItLight, _) = fromJust mLightIntersect
     maskingObject = (isJust mLightIntersect) && ((dItLight - d) < -epsilon)

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
