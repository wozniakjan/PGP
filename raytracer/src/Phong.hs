module Phong 
(
    phong,
    sum_phong,
) where

import Data_types
import Math
import Scene

sum_phong :: PhongColor -> Color
sum_phong (PhongColor d a s) = d+a+s

phong :: Intersection -> Dim3 -> Dim3 -> PhongColor
phong isec light_pos camera_pos = phong_color
    where
        phong_color = (PhongColor diffuse ambient specular)
        object_tex_type = texture (material (object isec)) 
        object_color 
            | object_tex_type == Procedural = checked_texture_procedural (point isec)   
            | object_tex_type == Checked = checked_texture (point isec) (shape (object isec))
            | object_tex_type == Bilinear = checked_bilinear_texture (point isec) (shape (object isec))
            | object_tex_type == Mip = checked_mip_texture (point isec) (shape (object isec))
            | otherwise = color (object isec)
        object_material = material (object isec)
        ambient = (Color 0.1 0.1 0.1) 
        diffuse = (n `dot` l)  `mul_color` object_color 
        specular = ((v `dot` r)**(30)) `mul_color` (Color 1 1 1)
        v = normalize ((point isec) `sub` camera_pos)
        n = normal isec
        l = normalize ((point isec) `sub` light_pos)
        r = normalize ((((2*(n `dot` l)) `mul` n)) `sub` l)
        dot x y 
            | x `dot_product` y <= 0 = 0
            | otherwise = x `dot_product` y
