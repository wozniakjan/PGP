-- Ray-tracer project PGR 2011
-- Authors: Jaroslav Cecho, xcecho00@stud.fit.vutbr.cz
--          Jan Wozniak, xwozni00@stud.fit.vutbr.cz

-- Phong light system module
   

module Phong
(
    PhongColor(PhongColor),
	phong,
    diffuse,
    ambient,
    specular,
    sumPhong,
) where

import Scene
import Math
import Materials
 
data PhongColor a = PhongColor { diffuse :: (Color a), ambient :: (Color a), specular :: (Color a) }

instance Num a => Num (PhongColor a) where
    (+) (PhongColor a b c) (PhongColor x y z) = PhongColor (a+x) (b+y) (c+z)
    (-) (PhongColor a b c) (PhongColor x y z) = PhongColor (a-x) (b-y) (c-z)
    (*) (PhongColor a b c) (PhongColor x y z) = PhongColor (a*x) (b*y) (c*z)
    abs = error "Not supported"
    signum = error "Not supported"
    fromInteger = error "Not supported"

instance Eq a => Eq (PhongColor a) where
    (==) (PhongColor a b c) (PhongColor x y z) = a == x && b == y && c == z
    
instance Show a => Show (PhongColor a) where
    show (PhongColor a b c) = "(PhongColor " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"
    
sumPhong :: Floating t => (PhongColor t) -> (Color t)
sumPhong (PhongColor d a s) = d + a + s

phong :: (Eq t, Floating t) => ParametrizedShape t -> Intersection t -> Vector t -> Material t -> Vector t -> PhongColor t
phong (PShape shape material) intersect lightPosition lightMaterial cameraPosition =  color
    where
        color = (PhongColor diffuse ambient specular)
        ambient = (getPhongAmbient material) * (getPhongAmbient lightMaterial)
        diffuse = multiplyColor ((getPhongDiffuse material) * (getPhongDiffuse lightMaterial)) (multiplyScalar normal location)
        specular = multiplyColor ((getPhongSpecular material) * (getPhongSpecular lightMaterial)) (cos**(getPhongShiny material))
        normal = normalizeVector (normalVector shape (getPoint intersect))
        location = normalizeVector ((getPoint intersect) - lightPosition)
        rebound = normalizeVector ((multiplyNum normal (2*(multiplyScalar normal location))) - location)
        toCamera = normalizeVector ((getPoint intersect) - cameraPosition)
        cos = (multiplyScalar rebound toCamera)
