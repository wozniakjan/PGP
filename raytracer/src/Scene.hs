module Scene (
    view_grid,
    camera_position,
    light_position,
    light_color,
    light_material,
    scene,
    black_color,
    red_color,
    background_color,
    checked_texture_procedural,
    checked_texture,
    checked_bilinear_texture,
    checked_mip_texture,
    view_grid_width,
    view_width,
    view_grid_height,
    step,
    bilinear,
) where

import Math
import Data_types

-- Frame buffer, projection of entire scene, first parameter is down left
-- corner, second parameter is up rigt corner. Returns grid of pixels
step :: Double
step = 0.5

get_frame :: Dim3 -> Dim3 -> [Dim3]
get_frame down_left@(Point x y z) up_right@(Point x' y' z') =
    [(Point xs ys z) | xs<-[x,(x+step)..x'], ys<-[y,(y+step)..y']]

view_grid = get_frame (Point 0 0 0) (Point view_grid_width view_grid_height 0)

camera_position :: Dim3
camera_position = (Point 50 50 (-50))

light_position :: Dim3
light_position = (Point 50 (-10) 30)

light_color :: PhongColor
light_color = PhongColor (Color 0.5 0.5 0.5) (Color 0.8 0.8 0.8) (Color 1.0 1.0 1.0)

light_material :: Material
light_material = Material 1 0 Plain

view_grid_width :: Double
view_grid_width = 100
view_width :: Int
view_width = round (view_grid_width/step + 1.0)

view_grid_height :: Double
view_grid_height = 100

scene :: [Object]
scene = [sphere_two, sphere_four, tri_one, tri_two]


--XXX_color :: Color
green_color = (Color 0.1 0.7 0.1)
red_color = (Color 0.7 0.1 0.1)
blue_color = (Color 0.1 0.1 0.7)
black_color = (Color 0 0 0)
white_color = (Color 0.8 0.8 0.8)
background_color = (Color 0 0 0)

checked_texture_procedural :: Dim3 -> Color
checked_texture_procedural p  
    | xeven `xor` yeven `xor` zeven = white_color  
    | otherwise = black_color 
    where
        xeven = even (truncate ((x p) / 2.0))
        yeven = even (truncate ((y p) / 2.0))
        zeven = even (truncate ((z p) / 2.0)) 

checked_2d :: (Double, Double) -> Color
checked_2d p
    | xeven `xor` yeven = white_color
    | otherwise = black_color
    where 
        xeven = even (truncate ((fst p)/20.0))
        yeven = even (truncate ((snd p)/20.0))

texture_size :: Double
texture_size = 1000

gen_tex1 :: Double -> Double -> [[Color]]
gen_tex1 size step = split_every ((round (size/step))+1) map_grid 
    where
        grid = [(x,y) | x<-[0,step..size], y<-[0,step..size]]
        map_grid = map (checked_2d) grid
g_tex1 = gen_tex1 texture_size 1

tex1 :: (Int, Int) -> Color
tex1 p = (g_tex1!!(fst p))!!(snd p)

get :: [[Color]] -> (Int, Int) -> Color
get tex p = tex!!(fst p)!!(snd p)
mip_tex1 = gen_tex1 texture_size 1
mip_tex2 = gen_tex1 texture_size 2
mip_tex3 = gen_tex1 texture_size 4
mip_tex4 = gen_tex1 texture_size 8

mip_tex :: (Double, Double) -> Double -> Color
mip_tex p distance 
    | distance < 20 = get mip_tex1 (mult_p texture_size)
    | distance < 125 = get mip_tex2 (mult_p (texture_size/2))
    | distance < 250 = get mip_tex3 (mult_p (texture_size/4))
    | otherwise = get mip_tex3 (mult_p (texture_size/8))
    where
        mult_p n = ( floor(n*(fst p)), floor(n*(snd p)))

bilinear :: (Double, Double) -> ((Int, Int) -> Color) -> Color
bilinear p f 
    | y1==y2 = cr1
    | otherwise = cp cr1 cr2
    where
        x = fst p
        y = snd p
        x1 = floor x
        x2 = ceiling x
        y1 = floor y
        y2 = ceiling y
        to_d a = fromIntegral a 
        cr1 = ((((to_d x2)-x)/((to_d x2)-(to_d x1)))`mul_color`(f (x1,y1))) + 
              (((x-(to_d x1))/((to_d x2)-(to_d x1)))`mul_color`(f (x2,y1)))
        cr2 = ((((to_d x2)-x)/((to_d x2)-(to_d x1)))`mul_color`(f (x1,y2))) + 
              (((x-(to_d x1))/((to_d x2)-(to_d x1)))`mul_color`(f (x2,y2)))
        cp c1 c2  = ((((to_d y2)-y)/((to_d y2)-(to_d y1)))`mul_color`(c1)) + 
                    (((y-(to_d y1))/((to_d y2)-(to_d y1)))`mul_color`(c2))


checked_texture :: Dim3 -> Shape -> Color
checked_texture p s = tex1 uv_i
    where
        uv = uv_map p s
        uv_i = (to_i (fst uv), to_i (snd uv))
        to_i x = floor (x*texture_size)

checked_bilinear_texture :: Dim3 -> Shape -> Color
checked_bilinear_texture p s = bilinear uv_i tex1
    where
        uv = uv_map p s
        uv_i = (to_i (fst uv), to_i (snd uv))
        to_i x = x*texture_size

checked_mip_texture :: Dim3 -> Shape -> Color
checked_mip_texture p s = mip_tex uv distance
    where
        uv = uv_map p s
        distance = mag p

--material
shiny_material = (Material 100 0.6 Plain)
matt_material = (Material 100 0.1 Plain)
checked_material = (Material 100 0 Checked)
checked_texture_material = (Material 100 0 Procedural)
checked_texture_bilinear_material = (Material 100 0 Bilinear)
checked_texture_mip_material = (Material 100 0 Mip)

sphere_one = (Object 
                (Sphere (Point 15 60 50) 30) 
                checked_material
                white_color
             )
sphere_two = (Object
                (Sphere (Point 15 60 50) 30)
                checked_texture_material
                white_color
             )
sphere_three = (Object
                (Sphere (Point 80 60 50) 30)
                checked_texture_bilinear_material
                white_color
               )
sphere_four = (Object
                (Sphere (Point 80 60 50) 30)
                checked_texture_mip_material
                white_color
               )
sphere_huge = (Object
                (Sphere (Point 180 50 50) 30)
                checked_texture_bilinear_material
                red_color
             )

tri_one = (Object
              (Triangle (Point 500 100 0) (Point (-500) 100 0) (Point 500 100 500))
              shiny_material
              green_color
           )
tri_two = (Object
              (Triangle (Point (500) 100 500) (Point (-500) 100 0) (Point (-500) 100 500))
              shiny_material
              green_color
           )
