module Math
(
    distance,
    get_intersections,
    add, sub, mul,
    cross_product, dot_product,
    split_every,
    
    normal_vector,
    normalize,
    squared_mag,
    roots
) where

import Data_types

epsilon :: Double
epsilon = 0.0001
 
split_every n = takeWhile (not.null) . map (take n) . iterate (drop n)

add :: Dim3 -> Dim3 -> Dim3
add a b = Vector ((x a)+(x b)) ((y a)+(y b)) ((z a)+(z b))

sub :: Dim3 -> Dim3 -> Dim3
sub a b = Vector ((x a)-(x b)) ((y a)-(y b)) ((z a)-(z b))

cross_product :: Dim3 -> Dim3 -> Dim3
cross_product (Vector x y z) (Vector x' y' z') = Vector (y*z'-z*y') (z*x'-x*z') (x*y'-y*x')

dot_product :: Dim3 -> Dim3 -> Double
dot_product (Vector x y z) (Vector x' y' z') = x*x' + y*y' + z*z'

mul :: Double -> Dim3 -> Dim3
mul w (Vector x y z) = Vector (w*x) (w*y) (w*z)
mul w (Point x y z) = Point (w*x) (w*y) (w*z)

squared_mag :: Dim3 -> Double
squared_mag (Vector x y z) = (x*x) + (y*y) + (z*z)

mag :: Dim3 -> Double
mag v = sqrt (squared_mag v) 

normalize :: Dim3 -> Dim3
normalize v
    | (mag v) /= 0 = (1/(mag v)) `mul` v
    | otherwise    = (Vector 0 0 0)

-- Returns distance of two points
distance :: Dim3 -> Dim3 -> Double
distance (Point x y z) (Point x' y' z') = sqrt( (x'-x)^2 + (y'-y)^2 + (z'-z)^2 )

neg :: Dim3 -> Dim3
neg (Vector x y z) = Vector (-x) (-y) (-z)
neg (Point x y z) = Point (-x) (-y) (-z)

-- Get normal vector for shape at given point
normal_vector :: Shape -> Dim3 -> Dim3
normal_vector (Plane a b c d) _ = Vector a b c
normal_vector (Triangle a b c) _ = normalize ((b `sub` a) `cross_product` (c `sub` a))
normal_vector (Sphere c r) p = normalize (c `sub` p)

-- Creates plane from triangle
plane_from_triangle :: Shape -> Shape
plane_from_triangle (Triangle k l m) = (Plane a b c d)
    where
        d = -(a * (x k) + b * (y k) + c * (z k))
        a = x cross
        b = y cross
        c = z cross
        cross = (k `sub` l) `cross_product` (k `sub` m)



roots :: Double -> Double -> Double -> [Double]
roots a b c 
    | disc <  0 = []
    | disc == 0 = [t1]
    | disc >  0 = [t1,t2] 
    where 
        t1 = (-b - sqrt_disc)/(2*a)
        t2 = (-b + sqrt_disc)/(2*a)
        disc = b^2 - 4*a*c
        sqrt_disc = sqrt disc

-- Returns list of intersection points 
-- sphere: (x-m)^2 + (y-n)^2 + (z-p)^2 = r^2
--         [x, y, z] coordinates of point on sphere
--         [m, n, p] coordinates of center point of sphere
-- ray: x_r = source_x + direction_x*t
--      y_r = source_y + direction_y*t
--      z_r = source_z + direction_z*t
-- intersect: (x_r-m)^2 + (y_r-n)^2 + (z_r-p)^2 = r^2
-- if (able to compute t) then intersect, else no intersect
-- intersect point: [x_r, y_r, z_r] where t is computed above
-- input: ray, shape
-- output: list of intersections
-- http://www.ccs.neu.edu/home/fell/CSU540/programs/RayTracingFormulas.htm
get_intersections :: Ray -> Object -> [Intersection]
get_intersections ray@(Ray dir source) obj@(Object sphere@(Sphere center radius) _) =
    map (get_intersect) points 
    where 
        a = squared_mag dir
        b = 2 * (dir `dot_product` (source `sub` center))
        c = (squared_mag (source `sub` center)) - radius^2
        points = map ((source `add`) . (`mul` dir)) (roots a b c) 
        get_intersect p = (Intersection p ray (normal_vector sphere p) obj)
