{-# LANGUAGE ScopedTypeVariables #-}

module GB2.RayTrace where

import GB2.Primitive
import GB2.Geometry
import GB2.Color
import GB2.Material
import GB2.Tonemap

import System.Random
import Control.Monad.Random
import Control.Monad (liftM, liftM2)

depthProba :: Float
depthProba = 0.7

goDeeper :: RandomGen g
         => Rand g Bool
goDeeper = do
 r <- getRandomR (0.0, 1.0)
 return $ r < depthProba


-- | Returns the energy transmitted by a given light at an intersection point. No more than 10 rebounds
directLighting :: Scene
               -> It
               -> Light
               -> Color
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

-- | indirectLighting calculation : no more than 10 rebounds
indirectLighting :: RandomGen g
                 => Scene
                 -> It
                 -> Light
                 -> Rand g Color
indirectLighting scene it light = do
 go <- goDeeper
 case go of
     False -> return color0
     True -> case (getObjectMaterial (getItObject it)) of
               -- Diffuse _ -> return $ Vector 255 255 255
               Diffuse _ -> do
                r1 <- getRandomR (0.0, 1.0)
                r2 <- getRandomR (0.0, 1.0)
                let x = 2 * cos(2 * pi * r1) * sqrt (r2 * (1 - r2))
                    y = 2 * sin(2 * pi * r1) * sqrt (r2 * (1 - r2))
                    z = (1 - 2 * r2)
                    randomDir = Vector x y z
                    rayDir = normalize $ case sameSide itRayDirToOrig randomDir itNormal of
                                             True -> randomDir
                                             False -> (- randomDir)
                    randomRay = Ray (itPoint + (epsilon `vmul2` rayDir)) rayDir
                rad <- (radianceRay scene light randomRay)
                return $ (materialAlbedo mat) * rad `vmul` (1.0 / pdf) `vmul` (abs (dot itNormal rayDir))
                  where
                   -- pdf of choosing a point uniformely on the hemisphere
                   pdf = 1.0 / (2.0 * pi)

               Glass _ ior -> do
                (r :: Float) <- getRandomR (0.0, 1.0)
                rad <- case refractedRayDir of
                     Nothing -> reflectedEnergy
                     Just direction -> case r < fresnel of
                                            True -> reflectedEnergy
                                            False -> (radianceRay scene light refractedRay)
                                              where
                                               refractPoint = itPoint + (epsilon `vmul2` direction)
                                               refractedRay = Ray refractPoint direction

                -- Here we have a sampling event of pdf = (fresnel) or (1 -
                -- fresnel) if we sample reflection or refraction
                -- Scattering equation also contains this term, so f / pdf = 1
                -- and hence we discard them of the computation for numerical stability.
                return (rad * (materialAlbedo mat))

                 where
                  fresnel = fresnelR itNormal itRayDirToOrig ior
                  -- Refraction
                  refractedRayDir = refract itNormal itRayDirToOrig ior
                  -- Reflection
                  reflectedEnergy = radianceRay scene light rayFromMirror

               Mirror _ -> do
                rad <- radianceRay scene light rayFromMirror
                return ((materialAlbedo mat) * rad)

               where
                  obj = (getItObject it)
                  mat = (getObjectMaterial obj)
                  itPoint@(Vector cx cy cz) = (getItPoint it)
                  itRayDirToOrig = (getItDirToRayOrig it)
                  itNormal = (getItNormal it)
                  reflectedDir = reflect itNormal itRayDirToOrig
                  -- We add an epsilon to move the point "away" from the sphere (floating point issues)
                  reflectPoint = itPoint + (epsilon `vmul2` reflectedDir)
                  rayFromMirror = Ray reflectPoint reflectedDir

-- The raytrace function
-- Display the color of the sphere hit by the ray
radianceRay :: RandomGen g
            => Scene
            -> Light
            -> Ray
            -> Rand g Color
radianceRay scene light ray = case intersectScene scene ray of
  Just (_, intersect) -> do
   indirectRad <- indirectLighting scene intersect light
   let directRad = directLighting scene intersect light
   return (directRad + indirectRad)
  Nothing -> return color0

radianceXY :: RandomGen g
           => Scene          -- ^ Scene to render
           -> Light          -- ^ Light
           -> Int            -- ^ number of samples
           -> (Float, Float) -- ^ (x, y)
           -> Rand g Color
radianceXY scene light spp coord =
  (liftM2 vmul2) (return (1.0 / (fromIntegral spp))) $
    foldl (liftM2 (+)) (return color0) [ radianceRay scene light (getCameraRay sample coord) | sample <- [0..(spp-1)] ]

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
render :: RandomGen g
       => Int       -- ^ width
       -> Int       -- ^ height
       -> Int       -- ^ spp
       -> Scene     -- ^ Scene to render
       -> Light     -- ^ Light in the scene
       -> Rand g [Color]
render width height spp scene light =
 liftM (fmap gamma22)
 (mapM
  (radianceXY scene light spp)
  (coordListPlane width height)
 )
