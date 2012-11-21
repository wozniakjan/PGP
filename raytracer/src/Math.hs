module Math
(
	Dim3(Vector,Point,Null),
) where

epsilon :: Double
epsilon = 0.0001
 
data Dim3 = Vector { x :: Double, y :: Double, z :: Double }
    | Point { x :: Double, y :: Double, z :: Double }
	| Null
	deriving (Eq, Show)

-- Vector and Point
data Ray = Ray { vector :: Dim3, source :: Dim3 }
    deriving (Eq, Show)

-- Triangle - Point, Point, Point, Point
-- Sphere - Center, Radius
data Shape = Triangle { a :: Dim3, b :: Dim3, c :: Dim3 }
    | Plane { k :: Double, l :: Double, m :: Double, n :: Double }
    | Sphere { c :: Dim3, r :: Double }
    deriving (Eq, Show)

data Intersection = Intersect { point :: Dim3, ray :: Ray, normal :: Dim3 }
    | NullIntersect
    deriving (Eq, Show)

add :: Dim3 -> Dim3 -> Dim3
add (Point x y z) (Point x' y' z') = Point (x+x') (y+y') (z+z')
add (Vector x y z) (Vector x' y' z') = Vector (x+x') (y+y') (z+z')
add Null _ = Null
add _ Null = Null

sub :: Dim3 -> Dim3 -> Dim3
sub (Point x y z) (Point x' y' z') = Point (x-x') (y-y') (z-z')
sub (Vector x y z) (Vector x' y' z') = Vector (x-x') (y-y') (z-z')
sub Null _ = Null
sub _ Null = Null

cross_product :: Dim3 -> Dim3 -> Dim3
cross_product (Vector x y z) (Vector x' y' z') = Vector (y*z'-z*y') (z*x'-x*z') (x*y'-y*x')
cross_product Null _ = Null
cross_product _ Null = Null

dot_product :: Dim3 -> Dim3 -> Double
dot_product v v' = (x v)*(x v') + (y v)*(y v') + (z v)*(z v')

mul :: Double -> Dim3 -> Dim3
mul w (Vector x y z) = Vector (w*x) (w*y) (w*z)
mul w (Point x y z) = Point (w*x) (w*y) (w*z)
mul w Null = Null

abs_dim3 :: Dim3 -> Double
abs_dim3 Null = 0
abs_dim3 v = sqrt((x v)^2+(y v)^2+(z v)^2)

squared_mag :: Dim3 -> Double
squared_mag (Vector x y z) = (x*x) + (y*y) + (z*z)

mag :: Dim3 -> Double
mag v = abs_dim3 v 

normalize :: Dim3 -> Dim3
normalize v
    | (mag v) /= 0 = (mag v) `mul` v
    | otherwise    = (Vector 0 0 0)

-- Returns distance of two points
distance :: Dim3 -> Dim3 -> Double
distance (Point x y z) (Point x' y' z') = sqrt((x'-x)^2 +(y'-y)^2 + (z'-z)^2)

neg :: Dim3 -> Dim3
neg Null = Null
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
        t1 = (-b - sqrt_disc)/2*a
        t2 = (-b + sqrt_disc)/2*a
        disc = b^2 - 4*a*c
        sqrt_disc = sqrt disc

-- Returns intersection point 
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
-- output: point of intersection
-- http://www.ccs.neu.edu/home/fell/CSU540/programs/RayTracingFormulas.htm
intersect :: Ray -> Shape -> Intersection
intersect ray@(Ray dir source) sphere@(Sphere center radius)
    | (disc >= 0) = closest_intersection
    | otherwise = NullIntersect
    where 
        intersectDirection = normalize (point `sub` source)
        a = squared_mag dir
        b = 2 * (dir `dot_product` (source `sub` center))
        c = (squared_mag (source `sub` center)) - radius^2
        disc = b^2 - 4*a*c
        t1 = (-b - sqrt_disc)/2*a
        t2 = (-b + sqrt_disc)/2*a
        sqrt_disc = sqrt disc
        point = source `add` (t1 `mul` dir)
        closest_intersection = (Intersect point ray (normal_vector sphere point))
