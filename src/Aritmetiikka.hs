module Aritmetiikka where
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

(#+) :: Point -> Vector -> Point
(a,b) #+ (x,y) = (a+x,b+y)

(#-) :: Point -> Point -> Vector
(a,b) #- (x,y) = (a-x,b-y)

