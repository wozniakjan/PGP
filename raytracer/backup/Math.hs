-- Ray-tracer project PGR 2011
-- Authors: Jaroslav Cecho, xcecho00@stud.fit.vutbr.cz
--          Jan Wozniak, xwozni00@stud.fit.vutbr.cz

-- Math module for ray-tracer


module Math
(
	Vector(Vector,NullVector),
	Shape(Triangle,Plane,Sphere,NullShape),
	Ray(Ray),
    Intersection(Intersect, NullIntersect),
	distance,
	getSourceFromRay,
    getVectorFromRay,
	intersect,
	getX,	getY,	getZ,
    getCenter, getRadius,
	normalVector,
	multiplyScalar,
	multiplyNum,
	normalizeVector,
	absVector,
    isRayIntersectingPlane,
    isPointInTriangle,
    getPoint,
    getRay,
    getNormal,
    neg,
    eps,
    isPointOnRay,
    squared_mag
) where

eps :: Double
eps = 0.2
 
-- One point (pixel, vertex, whatever what have 3 dimensions)
data Vector a = Vector { getX :: a, getY :: a, getZ :: a }
	| NullVector
	
instance Num a => Num (Vector a) where
	(+) (Vector a b c) (Vector x y z) = Vector (a+x) (b+y) (c+z)
	(-) (Vector a b c) (Vector x y z) = Vector (a - x) (b - y) (c - z)
 	(*) (Vector a b c) (Vector x y z) = Vector (b*z - c*y) (c*x - a*z) (a*y - b*x)
	abs = error "Not supported"
	signum = error "Not supported"
	fromInteger = error "Not supported"
	
instance Eq a => Eq (Vector a) where
	(==) (Vector a b c) (Vector x y z) = a == x && b == y && c == z
	(==) (Vector a b c) NullVector = False
	(==) NullVector (Vector a b c) = False
	(==) NullVector NullVector = True
	
instance Show a => Show (Vector a) where
	show (Vector a b c) = "(Vector " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"
	
	
isPointOnRay :: (Num a, Ord a) => Ray a -> Vector a -> Bool
isPointOnRay (Ray vector source) point = inDirection2 (getX) (>=) (<=) && inDirection2 (getY) (>=) (<=) && inDirection2 (getZ) (>=) (<=)
    where 
        inDirection (sign) v s p = if(v `sign` 0 && p `sign` s) then True else False
        inDirection1 (get) (sign) = inDirection (sign) ((get) vector) ((get) source) ((get) point)
        inDirection2 (get) (sign1) (sign2) = inDirection1 (get) (sign1) || inDirection1 (get) (sign2)
        
    
	 
-- Absolute value of vector
absVector :: Floating a => Vector a -> a
absVector (Vector a b c) = sqrt (a**2 + b**2 + c**2)

-- Ray
-- Consisting of two points (not vectors!)
--data Ray a = Ray { getDirectionFromRay :: (Vector a), getSourceFromRay :: (Vector a) }
--	deriving(Show,Eq)
-- Consisting of vector and source point
data Ray a = Ray { getVectorFromRay :: (Vector a), getSourceFromRay :: (Vector a) }
    deriving(Show,Eq)

-- Shapes that we are supporting
data Shape a = Triangle (Vector a) (Vector a) (Vector a)
    | Plane { getA :: a, getB :: a, getC :: a, getD :: a }
    | Sphere { getCenter :: (Vector a), getRadius :: a }
	| NullShape
	deriving(Show,Eq)
	

data Intersection a = Intersect { getPoint :: Vector a, getRay :: Ray a, getNormal :: Vector a}
    | NullIntersect { getPoint :: Vector a }
    deriving(Show,Eq)

-- Get normal vector for shape at given point
normalVector :: (Eq a, Floating a) => Shape a -> Vector a -> Vector a
normalVector (Plane a b c d) _ = Vector a b c
normalVector (Triangle a b c) _ = normalizeVector ((b - a) * (c - a))
normalVector (Sphere c r) p = normalizeVector (c - p)
--normalVector (Sphere c r) (Vector x y z) = Vector ((x - getX c)/r) ((y - getY c)/r) ((z - getZ c)/r)

-- Magnitude of Vector
squared_mag :: Floating a => Vector a -> a
squared_mag (Vector x y z) = (x*x) + (y*y) + (z*z)

mag :: Floating a => Vector a -> a
mag v = sqrt (squared_mag v)

-- Normalize vector
normalizeVector :: (Eq a, Floating a) => Vector a -> Vector a
normalizeVector v
    | (mag v) /= 0 = multiplyNum v (1 / mag v) 
    | otherwise    = (Vector 0 0 0)

   -- Create plane from three vectors (points)
-- 1) Create vectors kl and km
-- 2) Cross product km and km -- will get us a, b and c
-- 3) Calculate d based one point and cross product
planeFromTriangle :: Floating a => Shape a -> Shape a
planeFromTriangle (Triangle k l m) = (Plane a b c d)
    where
        d = - (a * (getX k) + b * (getY k) + c * (getZ k))
        a = getX cross
        b = getY cross
        c = getZ cross
        cross = kl * km
        kl = k - l
        km = k - m
        
        
-- Multiplication
multiplyScalar :: Floating a => Vector a -> Vector a -> a
multiplyScalar (Vector a b c) (Vector x y z) = a*x + b*y + c*z

multiplyNum :: Floating a => Vector a -> a -> Vector a
multiplyNum (Vector a b c) x = Vector (a*x) (b*x) (c*x)

-- Get distance of two vertexes
distance :: Floating a => Vector a -> Vector a -> a
distance NullVector _ = 1/0
distance _ NullVector = 1/0
distance (Vector a b c) (Vector x y z) = sqrt ((a - x)**2 + (b - y)**2 + (c - z)**2)

neg :: Floating a => Vector a -> Vector a
neg NullVector = NullVector
neg (Vector x y z) = (Vector (-x) (-y) (-z))

-- Returns true if given ray is intersecting given plane
--isPointInTriangle :: (Floating a, Ord a) => Vector a -> Shape a -> Bool
isRayIntersectingPlane :: (Eq a, Floating a) => Ray a -> Shape a -> Bool
isRayIntersectingPlane (Ray u b) plane = scalar /= 0
	where
		scalar = multiplyScalar u n
		n = normalVector plane NullVector {-- NullVector proto, ze je jedno v jakem bode se normala pocita --}

-- Returns true if given point is in triangle (must be on same plane!)
-- http://www.blackpawn.com/texts/pointinpoly/default.html
isPointInTriangle :: (Floating a, Ord a) => Vector a -> Shape a -> Bool
isPointInTriangle p (Triangle a b c) = (sameSide p a b c) && (sameSide p b a c) && (sameSide p c a b)
	where
		sameSide p k a b = (multiplyScalar mpa mka) >= 0
			where
				mpa = ba * pa
				mka = ba * ka
				ba = b - a
				pa = p - a
				ka = k - a

-- Returns intersection point
-- Plane: a*x + b*y + c*z + d = 0
-- Ray: X = A + k*u
-- Intersect: a(ax + k*ux) + b(ay + k*uy) + c(az + k*uz) + d = 0
-- k = ....
-- Intersect point: A + k*u
intersectRayWithPlane :: (Eq a, Floating a) => Ray a -> Shape a -> Vector a
intersectRayWithPlane (Ray rd r0) plane = point
	where
		k = numerator / denominator
		numerator = -((pn `multiplyScalar` r0) + d)
		denominator = pn `multiplyScalar` rd
		pn = normalVector plane NullVector
		d = getD plane
		point = Vector ((getX r0) + k*(getX rd)) ((getY r0) + k*(getY rd)) ((getZ r0) + k*(getZ rd))


-- Returns intersect point 
intersect :: Ray Double -> Shape Double -> Intersection Double
intersect ray (Triangle a b c) 
    | (isRayIntersectingPlane ray (planeFromTriangle (Triangle a b c)) && isPointInTriangle point (Triangle a b c)) && isPointOnRay ray point = intersection
    | otherwise = NullIntersect NullVector
        where
            pointDistance = distance (getSourceFromRay ray) point
            point = intersectRayWithPlane ray (planeFromTriangle (Triangle a b c))
            intersection | (pointDistance > eps) && True = Intersect point ray (normalVector (Triangle a b c) point) 
                         | otherwise = NullIntersect NullVector

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
--output: point of intersection
-- http://www.ccs.neu.edu/home/fell/CSU540/programs/RayTracingFormulas.htm
intersect (Ray dir source) (Sphere center radius) 
    | (disc >= 0) && isPointOnRay ray point  = intersection
    | otherwise = NullIntersect NullVector
    where
        intersectDirection = normalizeVector (point - source)
        a = squared_mag dir
        b = 2 * (dir `multiplyScalar` (source - center))
        c = (squared_mag (source - center)) - radius^2
        disc = b^2 - 4*a*c 
        t1 = (-b - (sqrt disc))/(2*a)
        t2 = (-b + (sqrt disc))/(2*a)
        point1 = source + (dir `multiplyNum` t1)
        point2 = source + (dir `multiplyNum` t2)
        distance1 = distance source point1 
        distance2 = distance source point2
        point | (distance1 <= eps) = point2
			  | (distance2 <= eps) = point1
              | otherwise = if(distance1 < distance2) then point1 else point2
        pointDistance | (distance1 <= eps) = distance2
                      | (distance2 <= eps) = distance1
                      | otherwise = if(distance1 < distance2) then distance1 else distance2
        intersection = if(pointDistance > eps) then (Intersect point ray (normalVector (Sphere center radius) point))
                                                                      else NullIntersect NullVector
        ray = Ray dir source
        
