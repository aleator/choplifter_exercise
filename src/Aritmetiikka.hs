module Aritmetiikka where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point

(#+) :: Point -> Vector -> Point
(a,b) #+ (x,y) = (a+x,b+y)


(#-) :: Point -> Point -> Vector
(a,b) #- (x,y) = (a-x,b-y)


