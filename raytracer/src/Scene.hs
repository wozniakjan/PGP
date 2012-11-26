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
    checked_texture,
    view_grid_width,
    view_width,
    view_grid_height,
    step
) where

import Math
import Data_types

-- Frame buffer, projection of entire scene, first parameter is down left
-- corner, second parameter is up rigt corner. Returns grid of pixels
step :: Double
step = 0.3

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
light_material = Material 1 0 0

view_grid_width :: Double
view_grid_width = 100
view_width :: Int
view_width = round (view_grid_width/step + 1.0)

view_grid_height :: Double
view_grid_height = 100

scene :: [Object]
scene = [sphere_one, sphere_two, tri_one, tri_two]


--XXX_color :: Color
green_color = (Color 0.1 0.7 0.1)
red_color = (Color 0.7 0.1 0.1)
blue_color = (Color 0.1 0.1 0.7)
black_color = (Color 0 0 0)
white_color = (Color 0.8 0.8 0.8)
background_color = (Color 0 0 0)

checked_texture :: Dim3 -> Color
checked_texture p  
    | xeven `xor` yeven `xor` zeven = white_color  
    | otherwise = black_color 
    where
        xeven = even (truncate ((x p) / 20.0))
        yeven = even (truncate ((y p) / 20.0))
        zeven = even (truncate ((z p) / 20.0)) 

--material
shiny_material = (Material 100 0.8 0)
matt_material = (Material 100 0 0)
checked_material = (Material 100 0.9 1)


sphere_one = (Object 
                (Sphere (Point 110 50 50) 30) 
                shiny_material
                green_color
             )
sphere_two = (Object
                (Sphere (Point 0 50 50) 30)
                matt_material
                red_color
             )

tri_one = (Object
              (Triangle (Point 500 100 0) (Point (-500) 100 0) (Point 500 100 500))
              checked_material
              white_color
           )
tri_two = (Object
              (Triangle (Point (500) 100 500) (Point (-500) 100 0) (Point (-500) 100 500))
              checked_material
              white_color
           )
