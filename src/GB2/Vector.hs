module GB2.Vector
(
    vec_add,
    vec_sub,
    vec_mul,
    vec_scalar_mul,
    vec_scalar_mul2,
    vec_length,
    vec_normalized,
    vec_dot,
    vec_cross,
    Vector
) where

type Vector = (Float, Float, Float)

vec_add :: Vector -> Vector -> Vector
vec_add (a, b, c) (d, e, f) = (a + d, b + e, c + f)

vec_sub :: Vector -> Vector -> Vector
vec_sub (a, b, c) (d, e, f) = (a - d, b - e, c - f)

vec_mul :: Vector -> Vector -> Vector
vec_mul (a, b, c) (d, e, f) = (a * d, b * e, c * f)

vec_scalar_mul :: Vector -> Float -> Vector
vec_scalar_mul (a, b, c) d = (a * d, b * d, c * d)

vec_scalar_mul2 :: Float -> Vector -> Vector
vec_scalar_mul2 d (a, b, c) = (a * d, b * d, c * d)

vec_length :: Vector -> Float
vec_length (a, b, c) = sqrt (a * a + b * b + c * c)

vec_normalized :: Vector -> Vector
vec_normalized (a, b, c) = (a / n, b / n, c / n) where n = vec_length (a, b, c)

vec_dot :: Vector -> Vector -> Float
vec_dot (a, b, c) (d, e, f) = a * d + b * e + c * f

vec_cross :: Vector -> Vector -> Vector
vec_cross (a, b, c) (d, e, f) = (b * f - c * e, c * d - a * f, a * e - b * d)

-- TODO: clean code, introduce real operators, make tests
