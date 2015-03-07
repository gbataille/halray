module GB2.Color
(
    Color,
    Image,
    color0,
    readColor,
    clampToInt255,
    image2ppm -- dump and image to disk
)
where

import GB2.Geometry

-- Define what is an image and what is a color

-- Input / Output
type Color = Vector
-- The pixels are stored in a flat array of width * height
type Image = (Int, Int, [Color]) -- Width, Height, Pixels

-- Constants
color0 :: Color
color0 = vector0

-- | In the context of ray tracing, a computation that "failed" i.e. did
-- not bring energy means actually rendering a black color
readColor :: Maybe Color -> Color
readColor Nothing = Vector 0 0 0
readColor (Just c) = c

-- Convert a floating point value color [0 1] ^ 3 to uint8 [0 255] ^ 3
color2list :: Color -> [Int]
color2list (Vector a b c) = [clampToInt255 (a * 255), clampToInt255 (b * 255), clampToInt255 (c * 255)]

clampToInt255 :: (RealFrac a) => a -> Int
clampToInt255 a
    | a <= 0 = 0
    | a > 255 = 255
    | otherwise = truncate a

color2string :: Color -> String
color2string color = unwords $ map show $ color2list color

pixels2string :: [Color] -> String
pixels2string pixels = unwords $ map color2string pixels

-- Convert an image to ppm string
image2ppm :: Image -> String
image2ppm (width, height, pixels) = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ pixels2string pixels ++ "\n"
