import System.Environment

import GB2.Geometry
import GB2.Primitive
import GB2.Material
import GB2.Color
import GB2.RayTrace

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

-- | 'main' runs the main program
main :: IO ()
main = do
  args <- getArgs
  let
    width = read (head args) :: Int
    height = read (head $ tail args) :: Int
    spp = read (head . tail $ tail args) :: Int
    res = render width height spp makeDefaultScene makeDefaultLight
    filename = "test.ppm"

  writeFile filename $ image2ppm (width, height, res)
