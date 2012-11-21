-- Ray-tracer project PGR 2011
-- Authors: Jaroslav Cecho, xcecho00@stud.fit.vutbr.cz
--          Jan Wozniak, xwozni00@stud.fit.vutbr.cz


import System.IO
import System.Environment
import Math
import Scene
import Raytracer
import Materials
import Png
import qualified Data.ByteString.Lazy.Char8 as B



main :: IO ()       
main = do
	B.writeFile "picture.png" (png (raytracer getScene getCamera getView getStepSize getLightPosition getLightMaterial))
	return () 
 