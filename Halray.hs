import System.Environment

import GB2.Geometry
import GB2.Primitive
import GB2.Material
import GB2.Color

-- Scene
-- Vec (50, 70, 81.6) // LIGHT.

-- The scene is composed of big spheres ;)
-- Note: find a way to be explicit about this list... This is ugly.

-- Scene intersect
makeDefaultScene :: Scene
makeDefaultScene = [
    Object (Sphere 100000.0 $ Vector (100000.0 + 1.0) 40.8 81.6) (Diffuse $ Vector 1.0 0.0 0),	-- Left
    Object (Sphere 100000.0 $ Vector (-100000.0 + 99.0) 40.8 81.6) (Diffuse $ Vector 0 0 1),	-- Rght
    Object (Sphere 100000.0 $ Vector 50 40.8 100000) (Diffuse $ Vector 1 0 1),	-- Back
    Object (Sphere 100000.0 $ Vector 50 100000.0 81.6) (Diffuse $ Vector 1 1 0),	-- Bottom
    Object (Sphere 100000.0 $ Vector 50 (-100000.0 + 81.6) 81.6) (Diffuse $ Vector 0 0 1),	-- Top
    Object (Sphere 16.5 $ Vector 27 16.5 47) (Mirror $ Vector 1 1 1),	-- Mirror
    Object (Sphere 16.5 $ Vector 73 16.5 78) (Diffuse $ Vector 1 1 1)	-- Glass
  ]

makeDefaultLight :: Light
makeDefaultLight = Light (Vector 50 70 81.6) (Vector 20000 20000 20000)

intersectToEnergy :: It -> Light -> Maybe Color
intersectToEnergy it light
  | sameSide incomingRay dirRay normalAtIntersect =
    Just ((bsdf material) * (getLightColor light) `vmul` (1.0/(d**2) )`vmul` (abs (dot normalAtIntersect dirRay)))
  | otherwise = Nothing
    where
      material = (getObjectMaterial $ getItObject it)
      intersectP = (getItPoint it)
      incomingRay = (getItDirToRayOrig it)
      dirRay = normalize ((getLightPosition light) - intersectP)
      lightRay = Ray (getLightPosition light) dirRay
      normalAtIntersect = (getItNormal it)
      d = norm (intersectP - (getLightPosition light))

-- The raytrace function
-- Display the color of the sphere hit by the ray
radiance :: Ray -> Color
radiance ray = case intersectScene makeDefaultScene ray of
                   Nothing -> Vector 0 0 0
                   Just (_, intersect) -> readColor $ intersectToEnergy intersect makeDefaultLight

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
render :: Int -> Int -> Int -> [Color] -- (width, height, spp)
render width height spp = [

                           -- sum (using fold) the "radiance" of the ray associated with the pixel
                           -- normalize by 1.0 / spp
                           (1.0 / (fromIntegral spp))
                           `vmul2`
                           (foldl (+) (Vector 0 0 0) [
                              -- call get_camera_ray with values in [-1 1] ^ 2
                              radiance $ get_camera_ray (2.0 * (fromIntegral x) / (fromIntegral width) - 1.0) (2.0 * (fromIntegral y) / (fromIntegral height) - 1.0) sample
                              -- foreach super sample
                              | sample <- [0..(spp-1)]])

                              -- Foreach pixels, compute a value
                              | y <- [0..(height-1)], x <- [0..(width-1)]]

-- | 'main' runs the main program
main :: IO ()
main = do
          args <- getArgs

          let
              width = read (head args) :: Int
              height = read (head $ tail args) :: Int
              spp = read (head . tail $ tail args) :: Int
              res = render width height spp
              filename = "test.ppm"

          writeFile filename $ image2ppm (width, height, res)
