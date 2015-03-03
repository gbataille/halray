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

-- | Returns the energy transmitted by a given light at an intersection point. No more than 10 rebounds
directLighting :: Scene
               -> It
               -> Int          -- ^ depth (i.e. number of indirect rebounds)
               -> Light
               -> Color
directLighting _ _ 10 _ = color0
directLighting scene it _ light
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
                 -> Int         -- ^ depth (i.e. number of indirect rebounds)
                 -> Light
                 -> Rand g Color
indirectLighting _ _ 10 _ = return color0
indirectLighting scene it depth light =
 case (getObjectMaterial (getItObject it)) of
      Diffuse _ -> return color0

      Glass _ ior -> do
       (r :: Float) <- getRandomR (0.0, 1.0)
       rad <- case r < fresnel of
                   True -> reflectedEnergy
                   False -> case refractedRayDir of
                                 Nothing -> return color0
                                 Just direction ->
                                   (radianceRay scene light (depth + 1) refractedRay)
                                     where
                                      refractPoint = itPoint + (epsilon `vmul2` direction)
                                      refractedRay = Ray refractPoint direction

       return (rad * (materialAlbedo mat))

        where
         obj = (getItObject it)
         mat = (getObjectMaterial obj)
         itNormal = (getItNormal it)
         itRayDirToOrig = (getItDirToRayOrig it)
         itPoint = (getItPoint it)
         fresnel = fresnelR itNormal itRayDirToOrig ior
         -- Refraction
         refractedRayDir = refract itNormal itRayDirToOrig ior
         -- Reflection
         reflectedDir = reflect itNormal itRayDirToOrig
         -- We add an epsilon to move the point "away" from the sphere (floating point issues)
         reflectPoint = itPoint + (epsilon `vmul2` reflectedDir)
         rayFromMirror = Ray reflectPoint reflectedDir
         reflectedEnergy = radianceRay scene light (depth + 1) rayFromMirror

      Mirror _ -> do
       rad <- radianceRay scene light (depth + 1) rayFromMirror
       return ((materialAlbedo mat) * rad)
        where
         obj = (getItObject it)
         mat = (getObjectMaterial obj)
         itPoint = (getItPoint it)
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
            -> Int      -- ^ depth (i.e. number of indirect rebounds)
            -> Ray
            -> Rand g Color
radianceRay scene light depth ray = case intersectScene scene ray of
  Just (_, intersect) ->
    (liftM2 (+)) (return (directLighting scene intersect depth light)) (indirectLighting scene intersect depth light)
  Nothing -> return color0

radianceXY :: RandomGen g
           => Scene          -- ^ Scene to render
           -> Light          -- ^ Light
           -> Int            -- ^ number of samples
           -> (Float, Float) -- ^ (x, y)
           -> Rand g Color
radianceXY scene light spp coord =
  (liftM2 vmul2) (return (1.0 / (fromIntegral spp))) $
    foldl (liftM2 (+)) (return color0) [ radianceRay scene light 0 (getCameraRay sample coord) | sample <- [0..(spp-1)] ]

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
