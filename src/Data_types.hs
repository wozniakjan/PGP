module Data_types(
	Dim3(Vector, Point),
    Ray(Ray),
    Shape(Triangle, Plane, Sphere),
    Intersection(Intersection),
    Object(Object),
    Color(Color),
    PhongColor(PhongColor),
    Material(Material),
    TextureType(Plain,Checked,Procedural,Bilinear,Mip),
    x, y, z, 
    vector, source, dest,
    a, b, c,
    k, l, m, n,
    r,
    point, ray, normal, object,
    shape, color, material, texture,
    red, green, blue,
    ambient, diffuse, specular, 
    shiny, reflectivity,
    average_colors,
    mul_color,
) where

data TextureType = Procedural | Bilinear | Mip | Checked | Plain
    deriving (Eq, Show, Ord)

data Dim3 = Vector { x :: Double, y :: Double, z :: Double }
    | Point { x :: Double, y :: Double, z :: Double }
	deriving (Eq, Show, Ord)

-- Vector and Point
data Ray = Ray { vector :: Dim3, source :: Dim3, dest :: Dim3 }
    deriving (Eq, Show, Ord)

-- Triangle - Point, Point, Point, Point
-- Sphere - Center, Radius
data Shape = Triangle { a :: Dim3, b :: Dim3, c :: Dim3 }
    | Plane { k :: Double, l :: Double, m :: Double, n :: Double }
    | Sphere { c :: Dim3, r :: Double }
    deriving (Eq, Show, Ord)

data Intersection = Intersection { point :: Dim3, ray :: Ray, normal :: Dim3, object :: Object }
    deriving (Eq, Show, Ord)

data Object = Object { shape :: Shape, material :: Material, color :: Color }
    deriving (Eq, Show, Ord)

data Color = Color { red :: Double, green :: Double, blue :: Double }
    deriving (Eq, Show, Ord)

instance Num Color where
    (+) (Color a b c) (Color x y z) = Color (sat (a+x)) (sat (b+y)) (sat (c+z))
    (-) (Color a b c) (Color x y z) = Color (sat (a-x)) (sat (b-y)) (sat (c-z))
    (*) (Color a b c) (Color x y z) = Color (sat (a*x)) (sat (b*y)) (sat (c*z))
    abs = error "N/A"
    signum = error "N/A"
    fromInteger = error "N/A"

instance Fractional Color where
    (/) (Color a b c) (Color x y z) = Color (a/x) (b/y) (c/z)
    fromRational = error "N/A"

data PhongColor = PhongColor { diffuse :: Color, ambient :: Color, specular :: Color } 
	deriving (Eq, Show, Ord)
    
instance Num PhongColor where
    (+) (PhongColor a b c) (PhongColor x y z) = PhongColor (a+x) (b+y) (c+z)
    (-) (PhongColor a b c) (PhongColor x y z) = PhongColor (a-x) (b-y) (c-z)
    (*) (PhongColor a b c) (PhongColor x y z) = PhongColor (a*x) (b*y) (c*z)
    abs = error "N/A"
    signum = error "N/A"
    fromInteger = error "N/A"

data Material = Material { shiny :: Double, reflectivity :: Double, texture :: TextureType}
    deriving(Show, Eq, Ord)

div_color :: Color -> Double -> Color
div_color (Color r g b) i = (Color (r/i) (g/i) (b/i))

mul_color :: Double -> Color -> Color
mul_color x (Color r g b) = (Color (x*r) (x*g) (x*b))

plus_non_sat :: Color -> Color -> Color
plus_non_sat (Color a b c) (Color x y z) = (Color (a+x) (b+y) (c+z))

average_colors :: [Color] -> Color
average_colors list = sum `div_color` count
    where
        sum = foldr (plus_non_sat) (Color 0 0 0) list
        count = fromIntegral (length list)

sat :: Double-> Double
sat a
    | a > 1.0 = 1.0
    | a < 0.0 = 0.0
    | otherwise = a

