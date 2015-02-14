module GB2.Material
(
    Material(..) -- Generic material
)
where

-- Materials
data Material = Diffuse Float Float Float | Mirror Float Float Float

-- Later, all the different material while inherite from Material and
-- implements different sampling/evaluating methods using something
-- which may looks like virtual dispatching. I have NO idea on how this
-- will work in Haskell
