-- Ray-tracer project PGR 2011
-- Authors: Jaroslav Cecho, xcecho00@stud.fit.vutbr.cz
--          Jan Wozniak, xwozni00@stud.fit.vutbr.cz

-- Materials module, converts Color to tuple for Png module


module Materials
(
-- Constructors
	Material(PhongMaterial),
	Color(Color),
-- Getters
	getPhongAmbient,
	getPhongDiffuse,
	getPhongSpecular,
	getPhongShiny,
    getReflectivity,
-- Playing with colors
	getColorTuple,
	normalizeColor,
	multiplyColor,
) where
 
-- Color
data Color a = Color a a a

instance Num a => Num (Color a) where
	(+) (Color a b c) (Color x y z) = Color (a+x) (b+y) (c+z)
	(-) (Color a b c) (Color x y z) = Color (a-x) (b-y) (c-z)
	(*) (Color a b c) (Color x y z) = Color (a*x) (b*y) (c*z)
	abs = error "Not supported"
	signum = error "Not supported"
	fromInteger = error "Not supported"

instance Eq a => Eq (Color a) where
	(==) (Color a b c) (Color x y z) = a == x && b == y && c == z

instance Show a => Show (Color a) where
	show (Color a b c) = "(Color " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"

multiplyColor :: Num a => Color a -> a -> Color a
multiplyColor (Color a b c) x = Color (a*x) (b*x) (c*x)

getColorTuple :: (Integral t, Integral t1, Integral t2, RealFrac a) => Color a -> (t, t1, t2)
getColorTuple (Color a b c) = ((round (a*255)), (round (b*255)), (round (c*255)))

normalizeColor :: (Fractional a, Ord a) => Color a -> Color a
normalizeColor (Color a b c) =  Color (normal a) (normal b) (normal c)
	where
    normal val = (if val > 1.0 then 1.0 else val)

-- Material structure for Phong model
data Material a = PhongMaterial 
    { getPhongAmbient :: (Color a), getPhongDiffuse :: (Color a), getPhongSpecular :: (Color a), getPhongShiny :: a, getReflectivity :: a }
        deriving(Show,Eq)
