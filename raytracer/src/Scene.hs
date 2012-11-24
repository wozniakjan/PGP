module Scene (
    view_grid,
    camera,
    light,
    scene,
    background_color,
    view_grid_width,
    view_width,
    view_grid_height
) where

import Math
import Data_types

-- Frame buffer, projection of entire scene, first parameter is down left
-- corner, second parameter is up rigt corner. Returns grid of pixels
get_frame :: Dim3 -> Dim3 -> [Dim3]
get_frame down_left@(Point x y z) up_right@(Point x' y' z') =
    [(Point xs ys z) | xs<-[x,(x+1)..x'], ys<-[y,(y+1)..y']]

view_grid = get_frame (Point 0 0 0) (Point view_grid_width view_grid_height 0)

camera :: Dim3
camera = (Point 10 50 (-50))

light :: Dim3
light = (Point 100 110 30)

view_grid_width :: Double
view_grid_width = 100
view_width :: Int
view_width = round (view_grid_width + 1.0)

view_grid_height :: Double
view_grid_height = 100

scene :: [Object]
scene = [sphere_one]


--XXX_color :: Color
green_color = (Color 0.1 0.7 0.1)
background_color = (Color 0 0 0)

sphere_one = (Object 
                (Sphere (Point 50 50 50) 40) 
                green_color
             )
