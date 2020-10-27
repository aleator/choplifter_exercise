module Talot where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

data Talo = Talo {talo_korkeus :: Float, talo_leveys :: Float
                 ,talo_sijainti :: Float }

piirräTalo :: Talo -> Picture
piirräTalo talo = let
                   paikoillaan = translate (talo_sijainti talo) (talo_korkeus talo / 2) talonKuva
                   talonKuva = color (greyN 0.5) 
                                (rectangleSolid (talo_leveys talo) (talo_korkeus talo))

                   ((vax,vay),(oyx,oyy)) = nurkkaPisteet talo 
                   apupisteet =  translate vax vay (color red (circleSolid 10))
                                <> translate oyx oyy (color red (circleSolid 10))
                  in paikoillaan <> apupisteet

nurkkaPisteet :: Talo -> (Point,Point)
nurkkaPisteet talo = 
    let
        vasenAla = (talo_sijainti talo - (talo_leveys talo / 2) , 0)
        oikeaYlä = (talo_sijainti talo + (talo_leveys talo / 2)      , talo_korkeus talo) 
    in (vasenAla,oikeaYlä)

osuukoTaloon :: Float -> Talo -> Float
osuukoTaloon kohta talo
    | abs (talo_sijainti talo - kohta) < (talo_leveys talo / 2) = talo_korkeus
      talo
    | otherwise = 0
