import System.Environment

import GB2.Geometry
import GB2.Primitive
import GB2.Material
import GB2.Color
import GB2.RayTrace

import System.Process
import System.IO (openFile, IOMode(..))
import System.Exit (ExitCode(..))

-- Scene
-- Vec (50, 70, 81.6) // LIGHT.

-- The scene is composed of big spheres ;)
-- Note: find a way to be explicit about this list... This is ugly.

-- Scene intersect
makeDefaultScene :: Scene
makeDefaultScene = [
    -- Left Wall
    Object (Triangle (Vector 0 0 0) (Vector 0 100 0) (Vector 0 0 150)) (Diffuse $ Vector 1.0 0 0),
    Object (Triangle (Vector 0 100 150) (Vector 0 100 0) (Vector 0 0 150)) (Diffuse $ Vector 1.0 0 0),
    -- Right Wall
    Object (Triangle (Vector 100 0 0) (Vector 100 100 0) (Vector 100 0 150)) (Diffuse $ Vector 0 0 1),
    Object (Triangle (Vector 100 100 150) (Vector 100 100 0) (Vector 100 0 150)) (Diffuse $ Vector 0 0 1),
    -- Back wall
    Object (Triangle (Vector 0 0 0) (Vector 100 0 0) (Vector 100 100 0)) (Diffuse $ Vector 1 0 1),
    Object (Triangle (Vector 0 0 0) (Vector 0 100 0) (Vector 100 100 0)) (Diffuse $ Vector 1 0 1),
    -- Bottom Floor
    Object (Triangle (Vector 0 0 0) (Vector 100 0 0) (Vector 100 0 150)) (Diffuse $ Vector 1 1 0),
    Object (Triangle (Vector 0 0 0) (Vector 0 0 150) (Vector 100 0 150)) (Diffuse $ Vector 1 1 0),
    -- Top Ceiling
    Object (Triangle (Vector 0 90 0) (Vector 100 90 0) (Vector 0 90 150)) (Diffuse $ Vector 0 1 1),
    Object (Triangle (Vector 100 90 150) (Vector 100 90 0) (Vector 0 90 150)) (Diffuse $ Vector 0 1 1),
    -- Mirror left Sphere
    Object (Sphere 16.5 $ Vector 27 16.5 47) (Mirror $ Vector 1 1 1),
    -- Opaque Right Sphere
    Object (Sphere 16.5 $ Vector 73 16.5 78) (Diffuse $ Vector 1 1 1)
  ]

makeDefaultLight :: Light
makeDefaultLight = Light (Vector 50 70 81.6) (Vector 15000 15000 15000)

-- | 'main' runs the main program
main :: IO ()
main = do
  args <- getArgs
  let
    width = read (head args) :: Int
    height = read (head $ tail args) :: Int
    spp = read (head . tail $ tail args) :: Int
    res = render width height spp makeDefaultScene makeDefaultLight
    ppmFilename = "test.ppm"
    jgpFilename = "test.jpg"

  putStrLn ""
  putStrLn "### Halray ###"
  putStrLn ""
  -- Creating the basic PPM file
  writeFile ppmFilename $ image2ppm (width, height, res)
  -- Tries to convert it to a JPG file
  -- TODO: gbataille - Windows compatibility --> /dev/null ≣ NUL && which
  devNull <- openFile "/dev/null" ReadWriteMode
  (_, _, _, procHandle) <- createProcess (proc "which" ["convert"]) { std_out = (UseHandle devNull) }
  ret <- waitForProcess procHandle
  case ret of
       ExitSuccess -> do
         putStrLn $ "ImageMagick (convert) is available, converting the generated image to a jpg called: " ++ jgpFilename
         convRet <- system $ "convert " ++ ppmFilename ++ " " ++ jgpFilename
         case convRet of
              ExitFailure _ -> putStrLn "ERROR: Failed to convert the PPM into a JPG"
              ExitSuccess -> do
                rmRet <- system $ "rm " ++ ppmFilename
                case rmRet of
                     ExitFailure _ -> putStrLn "ERROR: Failed to delete the PPM file after conversion"
                     ExitSuccess -> return ()
       ExitFailure _ -> do
         putStrLn "ImageMagick (convert) is not installed (or not in your PATH)."
         putStrLn $ "The output is a raw PPM image called: " ++ ppmFilename
         putStrLn "To get a JPG image, install ImageMagick"
