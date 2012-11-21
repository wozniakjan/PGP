-- Ray-tracer project PGR 2011
-- Authors: Jaroslav Cecho, xcecho00@stud.fit.vutbr.cz
--          Jan Wozniak, xwozni00@stud.fit.vutbr.cz

-- Scene module with hard coded scene and color constants

module Scene (
	Scene(Scene),
	View(View),
	Color(Color),
	ParametrizedShape(PShape,NullPShape),
	getViewFirstBoundary,
	getViewSecondBoundary,
	getShape,
    getMaterial,
    getScene,
    getCamera,
    getLightPosition,
    getLightMaterial,
    getView,
    getStepSize
) where

import Math
import Materials

-- Colored triangle
data ParametrizedShape a = PShape { getShape :: (Shape a), getMaterial :: (Material a) }
	| NullPShape
	deriving(Show,Eq)

-- Scene
-- Tree scene is not supported yet
data Scene a = Scene [ParametrizedShape a]
	deriving(Show,Eq)

-- View box
data View a = View { getViewFirstBoundary :: (Vector a), getViewSecondBoundary :: (Vector a) }

-- Hard coded scene
background1left = (Triangle ( Vector (-100.0 ) (30.0 ) (-30.0 )) (Vector ( -3.0 ) (-8.5 ) (-30.0 )) (Vector ( -100.0 ) ( -8.5 ) (-30.0 )) )
background2left = (Triangle ( Vector (-100.0 ) (30.0 ) (-30.0 )) (Vector ( -3.0 ) ( 30  ) (-30.0 )) (Vector (  -3.0 ) ( -8.5 ) (-30.0 )) )

background1right = (Triangle ( Vector (3.0 ) (30.0 ) (-30.0 )) (Vector ( 100.0 ) (-8.5 ) (-30.0 )) (Vector ( 3.0 ) ( -8.5 ) (-30.0 )) )
background2right = (Triangle ( Vector (3.0 ) (30.0 ) (-30.0 )) (Vector ( 100.0 ) ( 30  ) (-30.0 )) (Vector ( 100.0 ) ( -8.5 ) (-30.0 )) )

mirrorWall1left = (Triangle ( Vector (-15.0 ) (30.0 ) (0.0 ))  (Vector ( -15.0 ) ( -8.5 ) (-30.0 ))  (Vector ( -15.0 ) (-8.5 ) (0.0 )))
mirrorWall2left = (Triangle ( Vector (-15.0) (30.0 ) (-30.0 )) (Vector ( -15.0 ) ( -8.5 ) (-30.0 ))  (Vector ( -15.0 ) ( 30  ) (0.0 )))

mirrorWall1right = (Triangle ( Vector (15.0 ) (30.0 ) (0.0 )) (Vector ( 15.0 ) (-8.5 ) (0.0 ))   (Vector ( 15.0 ) ( -8.5 ) (-30.0 )) )
mirrorWall2right = (Triangle ( Vector (15.0) (30.0 ) (-30.0 )) (Vector ( 15.0 ) ( 30  ) (0.0 )) (Vector ( 15.0 ) ( -8.5 ) (-30.0 )) )

foreground1 = (Triangle (Vector ( 0.0 ) ( 5.0 ) (-2.0 )) (Vector ( 5.0 ) ( 0.0 ) ( 0.0 )) (Vector (-5.0 ) ( 0.0 ) ( 0.0 )))
foreground2 = (Triangle (Vector ( 0.0 ) ( 0.0 ) ( 0.0 )) (Vector ( 5.0 ) (-5.0 ) (-2.0 )) (Vector (-5.0 ) (-5.0 ) (-2.0 )))

greenMaterial      = PhongMaterial (Color 0.1 0.5 0.1) (Color 0.1 0.5 0.1) (Color 1.0 1.0 1.0) 30 0
blueMaterial       = PhongMaterial (Color 0.3 0.3 0.8) (Color 0.3 0.3 0.8) (Color 1.0 1.0 1.0) 30 0
redMaterial        = PhongMaterial (Color 1.0 0.0 0.0) (Color 1.0 0.0 0.0) (Color 1.0 1.0 1.0) 30 0
mirrorMaterial     = PhongMaterial (Color 0.1 0.1 0.1) (Color 0.1 0.1 0.1) (Color 1.0 1.0 1.0) 30 0.9
darkMirrorMaterial = PhongMaterial (Color 0.1 0.1 0.1) (Color 0.1 0.1 0.1) (Color 1.0 1.0 1.0) 30 0.5
greenMirrorMaterial = PhongMaterial (Color 0.0 1.0 0.0) (Color 0.0 1.0 0.0) (Color 1.0 1.0 1.0) 30 0.5
redMirrorMaterial = PhongMaterial (Color 1.0 0.0 0.0) (Color 1.0 0.0 0.0) (Color 1.0 1.0 1.0)  30 0.5
blueMirrorMaterial = PhongMaterial (Color 0.0 0.0 1.0) (Color 0.0 0.0 1.0) (Color 1.0 1.0 1.0) 30 0.5

sp = (Sphere {--center--} (Vector (-5.0 ) (-3.5 ) (-10.0 )) {-- radius--} (5.0 ) )
sp2 = (Sphere {--center--} (Vector (5.0 ) (5 ) (-5.0 )) {-- radius--} (4.0 ) )
sp3 = (Sphere (Vector (-3) (-5.5) (-4)) 3.0)
ground1 = (Triangle (Vector (-100.0 ) (-8.5 ) (0.0 )) (Vector (-100.0 ) (-8.5 ) (-100.0 )) (Vector (100.0 ) (-8.5 ) (0.0 )))
ground2 = (Triangle (Vector (-100.0 ) (-8.5 ) (-100.0 ))  (Vector (100.0 ) (-8.5 ) (-100.0 )) (Vector (100.0 ) (-8.5 ) (0.0 )) )

getScene :: Scene Double
getScene = Scene [
-- Spheres
--    -- Background
    (PShape background1left blueMaterial),
    (PShape background2left blueMaterial),
    (PShape background1right blueMaterial),
    (PShape background2right blueMaterial),
    (PShape mirrorWall1right darkMirrorMaterial),
    (PShape mirrorWall2right darkMirrorMaterial),
    (PShape mirrorWall1left darkMirrorMaterial),
    (PShape mirrorWall2left darkMirrorMaterial),

--    -- Foreground
    --(PShape foreground1 redMaterial),
-- Original triangles
    (PShape ground1 greenMaterial),
    (PShape ground2 greenMaterial),
    (PShape sp greenMirrorMaterial),
    (PShape sp2 redMirrorMaterial),
    (PShape sp3 blueMirrorMaterial)
    ] 


-- Hard coded camera position
getCamera :: Vector Double
getCamera = Vector 0 0 (10 ) 

-- Hard coded light position
getLightPosition :: Vector Double
getLightPosition = Vector 0 15 (1 )

getLightMaterial :: Material Double
getLightMaterial = PhongMaterial (Color 0.5 0.5 0.5) (Color 0.8 0.8 0.8) (Color 1.0 1.0 1.0) 1 0

-- Hard coded view Matrix
getView :: View Double
getView = View (Vector (-15 ) (15 ) 0) (Vector (15 ) (-15 ) 0)

getStepSize :: Double
getStepSize = eps