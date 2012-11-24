module Raytracer (
    raytracer
) where

import Math
import Data_types
import Scene

raytracer :: [Color]
raytracer = map (rt) view_grid
    where
        rt point = ray_trace (Ray (normalize (point `sub` camera)) camera) point

ray_trace :: Ray -> Dim3 -> Color
ray_trace ray@(Ray vector source) point
    | intersections == [] = background_color
    | otherwise           = (Color 0.1 0.7 0.1)-- color (object (get_first point intersections))
    where
        intersections = concat (map (get_intersections ray) scene)

get_first :: Dim3 -> [Intersection] -> Intersection
get_first point intersections = head intersections
