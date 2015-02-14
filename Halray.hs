import System.Environment
import Data.List

import GB2.Vector
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
    ((100000.0, (100000.0 + 1.0, 40.8, 81.6)), (1.0, 0, 0)),	-- Left
    ((100000.0, (-100000.0 + 99.0, 40.8, 81.6)), (0.25, 0.25, 0.75)),	-- Rght
    ((100000.0, (50, 40.8, 100000)), (0.75, 0.75, 0.75)),	-- Back
    ((100000.0, (50, 100000.0, 81.6)), (0, 0.75, 0.75)),	-- Bottom
    ((100000.0, (50, -100000.0 + 81.6, 81.6)), (0.75, 0.75, 0)),	-- Top
    ((16.5, (27, 16.5, 47)), vec_scalar_mul (1, 0, 1) 0.90),	-- Mirror
    ((16.5, (73, 16.5, 78)), vec_scalar_mul (0, 1, 1) 0.90)	-- Glass
  ]

-- The raytrace function
-- Display the color of the sphere hit by the ray
radiance :: Ray -> Color
radiance ray = case intersectScene makeDefaultScene ray of
                   Nothing -> (0, 0, 0)
                   Just (t, (_, mat)) -> mat

-- Camera bullshit, we will need to improve this
near :: Float
near = 1.0

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
get_camera_ray :: Float -> Float -> Integer -> Ray -- (x, y, sample) -> Ray
get_camera_ray x y _ = (p0, direction)
  where
    (fx, fy) = (x, -y)
    -- p0, on near is in [0, 100] ^ 2 at z = 140
    p0 = ((fx + 1.0) * 50.0, (fy + 1.0) * 50.0, 140)
    -- p1, on far is at [0, 100] ^ 2 scalled by fov and at z = -far
    -- yes, depth is inverted
    p1 = ((fx * fov + 1.0) * 50.0, (fy * fov + 1.0) * 50.0, -far)
    direction = vec_normalized (vec_sub p1 p0)

-- Render an image of size Width x Height, with Sample super sample
render :: Integer -> Integer -> Integer -> [Color] -- (width, height, spp)
render width height spp = [

                           -- sum (using fold) the "radiance" of the ray associated with the pixel
                           -- normalize by 1.0 / spp
                           vec_scalar_mul2 (1.0 / fromIntegral spp) (foldl vec_add (0, 0, 0) [
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
              width = read (head args) :: Integer
              height = read (head $ tail args) :: Integer
              spp = read (head . tail $ tail args) :: Integer
              res = render width height spp
              filename = "test.ppm"

          writeFile filename $ image2ppm (width, height, res)
