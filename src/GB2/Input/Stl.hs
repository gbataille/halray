module GB2.Input.Stl
  (stlRead)
where

import GB2.Geometry

import Control.Exception.Base

read1point :: String -> Vector
read1point s = Vector (read x :: Float) (read y :: Float) (read z :: Float)
           -- a vertex is a ligne such as "vertex Float Float Float"
           -- unpack this
           where [_, x, y, z] = words s

parse1Triangle :: [String] -> Triangle
parse1Triangle linesContent = Triangle (read1point a) (read1point b) (read1point c)
         where
           -- A triangle is 7 lines, only three of them contains the vertex
           (_:_:a:b:c:_:_) = linesContent

parseNTriangles :: [String] -> Int -> [Triangle]
parseNTriangles linesContent n
                | n == 0 = assert (length linesContent == 1) []
                | otherwise = parse1Triangle linesContent:parseNTriangles (drop 7 linesContent) (n - 1)

stlRead :: FilePath -- ^ the path of the file containing STL data
        -> IO [Triangle] -- ^ the triangle list
stlRead filename = do
  contents <- readFile filename
  let l = lines contents
      len = length l
      nbTriangles = (len - 2) `div` 7

  -- tail remove the first line which is a free comment
  return (parseNTriangles (tail l) nbTriangles)
