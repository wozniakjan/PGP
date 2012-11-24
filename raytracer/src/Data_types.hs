module Data_types(
	Dim3(Vector, Point),
    Ray(Ray),
    Shape(Triangle, Plane, Sphere),
    Intersection(Intersection),
    Object(Object),
    Color(Color),
    x, y, z, 
    vector, source,
    a, b, c,
    k, l, m, n,
    r,
    point, ray, normal, object,
    shape, color,
    red, green, blue
) where


data Dim3 = Vector { x :: Double, y :: Double, z :: Double }
    | Point { x :: Double, y :: Double, z :: Double }
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

data Intersection = Intersection { point :: Dim3, ray :: Ray, normal :: Dim3, object :: Object }
    deriving (Eq, Show)

data Object = Object { shape :: Shape, color :: Color }
    deriving (Eq, Show)

data Color = Color { red :: Float, green :: Float, blue :: Float }
    deriving (Eq, Show)
