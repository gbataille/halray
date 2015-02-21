module GB2.RayTrace where

import GB2.Primitive
import GB2.Geometry
import GB2.Color
import GB2.Material

intersectToEnergy :: It -> Light -> Color
intersectToEnergy it light
  | sameSide incomingRay dirRay normalAtIntersect =
    ((bsdf material) * (getLightColor light) `vmul` (1.0/(d**2) )`vmul` (abs (dot normalAtIntersect dirRay)))
  | otherwise = Vector 0 0 0
    where
      material = (getObjectMaterial $ getItObject it)
      intersectP = (getItPoint it)
      incomingRay = (getItDirToRayOrig it)
      dirRay = normalize ((getLightPosition light) - intersectP)
      normalAtIntersect = (getItNormal it)
      d = norm (intersectP - (getLightPosition light))

-- The raytrace function
-- Display the color of the sphere hit by the ray
radiance :: Ray -> Scene -> Light -> Color
radiance ray scene light = case intersectScene scene ray of
                                Nothing -> Vector 0 0 0
                                Just (_, intersect) -> intersectToEnergy intersect light

-- Camera bullshit, we will need to improve this
near :: Float
near = 140

far :: Float
far = 10000.0

fov :: Float
fov = 40

-- The idea is simple, but complexe to apply.
-- We have two planes, on called "Near" at z = near and far at z = far.
-- First plane is between 0 and 100 for x and y
-- second plane is between 0 and 100 times fov for x and y.

-- If fov is 1, all rays are parallel
-- if fov is > 1, rays diverges and gives the perspective effect ;)

-- Generate camera ray
-- (x and y are in an unit cube of [-1, 2] ^ 2
-- sample is not used yet, it is for super sampling of the pixel
get_camera_ray :: Float -> Float -> Int -> Ray -- (x, y, sample) -> Ray
get_camera_ray x y _ = (Ray p0 direction)
  where
    (fx, fy) = (x, -y)
    -- p0, on near is in [0, 100] ^ 2 at z = 140
    p0 = Vector ((fx + 1.0) * 50.0) ((fy + 1.0) * 50.0) near
    -- p1, on far is at [0, 100] ^ 2 scalled by fov and at z = -far
    -- yes, depth is inverted
    p1 = Vector ((fx * fov + 1.0) * 50.0) ((fy * fov + 1.0) * 50.0) (-far)
    direction = normalize (p1 - p0)

-- Render an image of size Width x Height, with Sample super sample
render :: Int -> Int -> Int -> Scene -> Light -> [Color] -- (width, height, spp)
render width height spp scene light = [

                           -- sum (using fold) the "radiance" of the ray associated with the pixel
                           -- normalize by 1.0 / spp
                           (1.0 / (fromIntegral spp))
                           `vmul2`
                           (foldl (+) (Vector 0 0 0) [
                              -- call get_camera_ray with values in [-1 1] ^ 2
                              radiance (get_camera_ray (2.0 * (fromIntegral x) / (fromIntegral width) - 1.0) (2.0 * (fromIntegral y) / (fromIntegral height) - 1.0) sample) scene light
                              -- foreach super sample
                              | sample <- [0..(spp-1)]])

                              -- Foreach pixels, compute a value
                              | y <- [0..(height-1)], x <- [0..(width-1)]]

