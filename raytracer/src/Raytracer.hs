module Raytracer (
    raytracer,
    get_shade
) where

import Math
import Data_types
import Scene
import Phong

get_depth :: Int
get_depth = 3

raytracer :: [Color]
raytracer = map (rt) view_grid
    where
        rt point = ray_trace get_depth (Ray (normalize (point `sub` camera_position)) camera_position point)

ray_trace :: Int -> Ray -> Color
ray_trace depth ray@(Ray vector source dest) 
    | on_ray_intersections == [] = background_color
    | otherwise = (sum_phong (apply_shade get_phong)) + reflection_color 
    where
        all_intersections = concat (map (get_intersections ray) scene)
        intersections = map_filter_distance dest all_intersections
        on_ray_intersections = filter ((is_point_on_ray ray) . (point . snd)) intersections
        closest_intersection = snd (minimum on_ray_intersections) 
        get_phong = phong closest_intersection camera_position light_position
        shade = get_shade (point closest_intersection)
        apply_shade (PhongColor a d s) = 
            (PhongColor a ((fst shade) `mul_color` d) ((snd shade) `mul_color` s))
        --obj_reflectivity = reflectivity (material (object closest_intersection))
        reflection_color = reflection (depth-1) closest_intersection

map_filter_distance :: Dim3 -> [Intersection] -> [(Double, Intersection)]
map_filter_distance p il = filter ((>eps) . (fst)) (map (get_distance p) il)
    where
        get_distance p i = (distance p (point i), i)

is_on_ray :: Ray -> Intersection -> Bool
is_on_ray ray (Intersection p _ _ _) 
    | seg_a > eps && seg_b > eps = (ray_len > seg_a && ray_len > seg_b)
    | otherwise = False
    where
        ray_len = distance (source ray) (dest ray)
        seg_a = distance (source ray) p
        seg_b = distance (dest ray) p


-- Returns coeficient of pixel color whether it is in shade or not
-- fst - 0.2 if pixel is in shade (darker color)
-- fst - 1 if pixel is not in shade (color stays the same)
-- snd - 0 is in shade
-- snd - 1 is not in shade
get_shade :: Dim3 -> (Double, Double)
get_shade point
    | intersects == [] = (1, 1)
    | otherwise = (0.2, 0.0)
    where
        ray = (Ray (light_position `sub` point) point light_position)
        all_intersections = concat (map (get_intersections ray) scene)
        intersects = filter (is_on_ray ray) all_intersections


reflection :: Int -> Intersection -> Color			
reflection 0 _ = black_color
reflection depth intersection
    | refl_coef == 0 = black_color
    | otherwise = refl_coef `mul_color` reflected_color
    where
--        refl_coef = reflectivity (material (object intersection))
--        n = normalize (normal intersection)
--        v = normalize (neg(vector (ray intersection)))
--        k = 2*(n `dot_product` v)
--        p = (point intersection)
--        reflected_color = ray_trace depth out_ray
--        out_ray = (Ray n p (p `add` n))
        refl_coef = reflectivity (material (object intersection))
        k = 2 * (n `dot_product` v)
        n = normalize (normal intersection)
        v = normalize (neg(vector (ray intersection)))
        out_ray_dir = normalize ((mul k n) `sub` v)
        p = (point intersection)
        refl_ray = Ray out_ray_dir p (p `add` out_ray_dir)
        reflected_color = ray_trace depth refl_ray


is_point_on_ray :: Ray -> Dim3 -> Bool
is_point_on_ray (Ray vector source _) point = 
    in_dir2 (x) (>=) (<=) && in_dir2 (y) (>=) (<=) && in_dir2 (z) (>=) (<=)
    where 
        in_dir (sign) v s p = if(v `sign` 0 && p `sign` s) then True else False
        in_dir1 (get) (sign) = in_dir (sign) ((get) vector) ((get) source) ((get) point)
        in_dir2 (get) (sign1) (sign2) = in_dir1 (get) (sign1) || in_dir1 (get) (sign2)


--trace_reflect :: Int -> Ray -> Object -> Color
--trace_reflect depth ray@(Ray vector source dest) exclude
--    | excluded_intersections == [] = background_color
--    | otherwise = reflection_color + (sum_phong (apply_shade get_phong))
--    where
--        all_intersections = concat (map (get_intersections ray) scene)
--        intersections = map_filter_distance dest all_intersections
--        excluded_intersections = filter ((is_point_on_ray ray) . (point . snd)) intersections
--        closest_intersection = snd (minimum excluded_intersections) 
--        get_phong = phong closest_intersection camera_position light_position
--        shade = get_shade (point closest_intersection)
--        apply_shade (PhongColor a d s) = 
--            (PhongColor a ((fst shade) `mul_color` d) ((snd shade) `mul_color` s))
--        reflection_color = reflection (depth-1) closest_intersection
