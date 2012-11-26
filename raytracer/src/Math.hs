module Math
(
    distance,
    get_intersections,
    add, sub, mul,
    cross_product, dot_product,
    split_every,
    eps,
    neg,
    xor,

    normal_vector,
    normalize,
    squared_mag,
    roots
) where

import Data_types

eps :: Double
eps = 1.1

xor :: Bool -> Bool -> Bool
xor True b  = not b
xor False b = b

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
distance p p' = sqrt( ((x p')-(x p))^2 + ((y p')-(y p))^2 + ((z p')-(z p))^2 )

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

-- Returns true if given ray is intersecting given plane
--isPointInTriangle :: (Floating a, Ord a) => Vector a -> Shape a -> Bool
is_ray_intersecting_plane :: Ray -> Shape -> Bool
is_ray_intersecting_plane (Ray u b _) plane = scalar /= 0
	where
		scalar = u `dot_product` n
		n = normal_vector plane (Point 0 0 0)

-- Returns true if given point is in triangle (must be on same plane!)
-- http://www.blackpawn.com/texts/pointinpoly/default.html
is_point_in_triangle :: Dim3 -> Shape -> Bool
is_point_in_triangle p (Triangle a b c) = (same_side p a b c) && (same_side p b a c) && (same_side p c a b)
	where
		same_side p k a b = (mpa `dot_product` mka) >= 0
			where
				mpa = ba `cross_product` pa
				mka = ba `cross_product` ka
				ba = b `sub` a
				pa = p `sub` a
				ka = k `sub` a

-- Returns intersection point
-- Plane: a*x + b*y + c*z + d = 0
-- Ray: X = A + k*u
-- Intersect: a(ax + k*ux) + b(ay + k*uy) + c(az + k*uz) + d = 0
-- k = ....
-- Intersect point: A + k*u
intersect_ray_with_plane :: Ray -> Shape -> Dim3
intersect_ray_with_plane (Ray rd r0 _) plane = point
	where
        k' = numerator / denominator
        r0' = (Vector (x r0) (y r0) (z r0))
        numerator = -((pn `dot_product` r0') + d')
        denominator = pn `dot_product` rd
        pn = normal_vector plane (Point 0 0 0)
        d' = n plane
        point = (Point ((x r0) + k'*(x rd)) ((y r0) + k'*(y rd)) ((z r0) + k'*(z rd)))

-- Returns intersect points
get_intersections :: Ray -> Object -> [Intersection]
get_intersections ray obj@(Object triangle@(Triangle a b c) _ _) 
    | is_ray_insecing_plane && is_point_in_triangle point (triangle) = [intersection]
    | otherwise = []
    where
        is_ray_insecing_plane = is_ray_intersecting_plane ray (plane_from_triangle triangle) 
        point = intersect_ray_with_plane ray (plane_from_triangle triangle)
        intersection  = (Intersection point ray (normal_vector triangle point) obj)
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
-- get_intersections :: Ray -> Object -> [Intersection]
get_intersections ray@(Ray dir source dest) obj@(Object sphere@(Sphere center radius) _ _) =
    map (get_intersect) points 
    where 
        a = squared_mag dir
        b = 2 * (dir `dot_product` (source `sub` center))
        c = (squared_mag (source `sub` center)) - radius^2
        points = (map ((source `add`) . (`mul` dir)) (roots a b c)) 
        get_intersect p = (Intersection p ray (normal_vector sphere p) obj)
