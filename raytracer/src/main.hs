import System.IO
import System.Environment
import Math
import Scene
import Raytracer
import Png
import Data_types
import qualified Data.ByteString.Lazy.Char8 as B

color_to_tuple :: Color -> (Int, Int, Int)
color_to_tuple (Color r g b) = (round(r*255.0), round(g*255.0), round(b*255.0)) 


ascii :: [(Int, Int, Int)] -> String
ascii [] = "\n"
ascii ((0, 0, 0):t) = " " ++ ascii t
ascii (v:t) = "X" ++ ascii t

ascii_art :: [[(Int, Int, Int)]] -> String
ascii_art []     = "n"
ascii_art (h:t)  = (ascii h) ++ (ascii_art t)

main = B.writeFile "picture.png" (png (convert (raytracer)))
--main = writeFile "asci" (ascii_art (convert (raytracer)))
    where 
        convert l = split_every (view_width) (map (color_to_tuple) l)

