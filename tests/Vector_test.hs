import Test.QuickCheck
import Vector

main :: IO ()
main = quickCheck prop_vec_add

-- Adding a 0 vector is the identity
prop_vec_add :: Vector -> Bool
prop_vec_add v = vec_add v (0, 0, 0) == v

-- tests  = [("vec_add.0/id", quickCheck prop_vec_add)]
