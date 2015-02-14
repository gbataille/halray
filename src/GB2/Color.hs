module GB2.Color
(
    Color, 
    Image,
    image2ppm -- dump and image to disk
)
where

-- Define what is an image and what is a color

-- Input / Output
type Color = (Float, Float, Float) -- R, G, B

-- The pixels are stored in a flat array of width * height
type Image = (Integer, Integer, [Color]) -- Width, Height, Pixels

-- Convert a floating point value color [0 1] ^ 3 to uint8 [0 255] ^ 3
color2list :: Color -> [Integer]
color2list (a, b, c) = [truncate (a * 255), truncate (b * 255), truncate (c * 255)]

color2string :: Color -> String
color2string color = unwords $ map show $ color2list color

pixels2string :: [Color] -> String
pixels2string pixels = unwords $ map color2string pixels

-- Convert an image to ppm string
image2ppm :: Image -> String
image2ppm (width, height, pixels) = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ pixels2string pixels









