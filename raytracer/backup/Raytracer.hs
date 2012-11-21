-- Ray-tracer project PGR 2011
-- Authors: Jaroslav Cecho, xcecho00@stud.fit.vutbr.cz
--          Jan Wozniak, xwozni00@stud.fit.vutbr.cz

-- Ray-tracing module, here all the magic happens when rays are being cast from camera through pixel grid

module Raytracer (
	raytracer,
    getShade,
    reflection,   
) where

import Math
import Scene
import Phong
import Materials

whiteColor = (Color 1.0 1.0 1.0)
blackColor = (Color 0.0 0.0 0.0)
greenColor = (Color 0.0 1.0 0.0)

-- Casting rays
raytracer :: (Integral t, Integral t1, Integral t2) => Scene Double -> Vector Double -> View Double -> Double -> Vector Double -> Material Double -> [[(t, t1, t2)]]
raytracer scene camera (View viewFirstBoundary viewSecondBoundary) step lightPosition lightMaterial = kick viewFirstBoundary
   	where
		kick (Vector x y z)
			| y < (getY viewSecondBoundary) = []
			| otherwise = (row (Vector x y z)):(kick (Vector x (y-step) z))
		row (Vector x y z)
			| x > (getX viewSecondBoundary) = []
			| otherwise = (cast (Vector x y z)):(row (Vector (x+step) y z))
		cast point = getColorTuple (intersectRayWithScene 4 (Ray (normalizeVector (point-camera)) point) scene)

-- Get intersect vector with ray and scene
intersectRayWithScene :: Int -> Ray Double -> Scene Double -> Color Double
intersectRayWithScene depth ray (Scene xs) = check (1/0) (Color 0 0 0) xs
	where
		check minDistance minTriangle [] = minTriangle
		check minDistance minTriangle (x:xs) =
			if myDistance < minDistance
				then check myDistance sumColor xs
				else check minDistance minTriangle xs
			where
				sumColor = normalizeColor color 
				diffuseColor = (diffuse phongColor) `multiplyColor` shadeColor
				specularColor = (specular phongColor) `multiplyColor` isInShade
				ambientColor = ambient phongColor
				phongColor = phong x myIntersect getLightPosition getLightMaterial (getSourceFromRay ray)
				shadeColor = fst (getShade getLightPosition intersectPoint getScene) --coeficient <0;1> 1 means not in shade, 0 means totally covered
				isInShade = snd (getShade getLightPosition intersectPoint getScene)
				reflectionColor = reflection (depth-1) reflCoeficient myIntersect
				reflCoeficient = getReflectivity (getMaterial x)
				color = diffuseColor + ambientColor + reflectionColor + specularColor
				myDistance =  distance (getSourceFromRay ray) intersectPoint
				myIntersect = intersect ray (getShape x)
				intersectPoint = getPoint myIntersect

reflection :: Int -> Double -> Intersection Double -> Color Double			
reflection _ 0.0 _ = blackColor
reflection 0 _ _ = blackColor
reflection depth reflCoeficient intersection 
    | True  = reflectedColor `multiplyColor` reflCoeficient
    | otherwise = blackColor
    where 
        k = 2 * (n `multiplyScalar` v)
        n = normalizeVector (getNormal intersection)
        v = normalizeVector (neg(getVectorFromRay (getRay intersection)))
        outRayDir = normalizeVector ((multiplyNum n k) - v)
        reflectedColor = (intersectRayWithScene depth (Ray outRayDir (getPoint intersection)) getScene)
  

-- Returns coeficient of pixel color whether it is in shade or not
-- fst - 0.2 if pixel is in shade (darker color)
-- fst - 1 if pixel is not in shade (color stays the same)
-- snd - 0 is in shade
-- snd - 1 is not in shade
getShade :: (Fractional t, Fractional t1) => Vector Double -> Vector Double -> Scene Double -> (t, t1)
getShade _ _ (Scene []) = (1.0, 1.0)
getShade lightPosition point (Scene (x:xs))
    | isVisible = getShade lightPosition point (Scene xs)
    | otherwise = (0.2, 0.0)
         where
             isVisible = ((distance intersectPoint lightPosition) + eps) >= (distance lightPosition point)
             ray = (Ray (lightPosition-point) point)
             intersectPoint = getPoint (intersect ray (getShape x))
