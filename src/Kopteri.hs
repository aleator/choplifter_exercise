module Kopteri where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Geometry.Angle

import Aritmetiikka
import Hemmot(Hemmo)

luoKopteri :: Point -> Kopteri
luoKopteri paikka = Kopteri
       paikka 
       (0,0) 
       0 
       0
       0 

data Kopteri = Kopteri {
     kop_paikka :: Point -- ^ Missä kopteri?
    ,kop_nopeus :: Vector -- ^ Kuinka nopeasti menee?
    ,kop_teho   :: Float          -- ^ Teho
    ,kop_kulma  :: Float          -- ^ Kuinka vinossa
    ,kop_hemmojaKyydissä :: Natural -- Kuinka monta hemmoa kerätty 

}


päivitäKopteri :: Float -> Kopteri -> Kopteri
päivitäKopteri aikaEdellisestä kopteri = kopteri{
                            kop_paikka = (kopteriX+ aikaEdellisestä *  vX
                                         , max 0 (kopteriY+aikaEdellisestä *  vY) )
                           ,kop_nopeus = ((vX + dX) * 0.97 , (vY + dY - 5) * 0.97 )
                            }
            where 
                (dX,dY) = kulmaJaTehoKiihtyvyydeksi (kop_teho kopteri) (kop_kulma kopteri)
                (kopteriX,kopteriY) = kop_paikka kopteri
                (vX,vY) = kop_nopeus kopteri

noukiHemmot :: [Hemmo] -> Kopteri -> Kopteri
noukiHemmot hemmot kopteri 
    = kopteri{
       kop_hemmojaKyydissä = (kop_hemmojaKyydissä kopteri + genericLength hemmot)
      }

kulmaJaTehoKiihtyvyydeksi :: Float -> Float -> (Float,Float)
kulmaJaTehoKiihtyvyydeksi teho kulma 
    = rotateV (- degToRad kulma) (0,teho) 

piirräKopteri :: Float -> Float -> Picture
piirräKopteri teho aika = translate 0 (150) (color white runko)
 where
  runko = circleSolid 100 
            <> translate (-200) 0 (rectangleSolid 300 30)
            <> translate (-350) 0 (rectangleSolid 50 100)
            <> lapa 
            <> translate 0 90     (rectangleSolid 10 120)

            <> translate (-50) (-90)     (rectangleSolid 10 120)
            <> translate (50) (-90)      (rectangleSolid 10 120)
            <> translate 0 (-150)        (rectangleSolid 200 15)

  lapa = translate 0 150 (rectangleSolid (350 * sin (aika * teho)) 10)

kopteriTörmäysviivat :: Point -> Float -> ((Point,Point) , (Point,Point))
kopteriTörmäysviivat paikka kulma = 
    let
     vasen = -170
     oikea = 100 
     kääntö = rotateV (- degToRad kulma)
    in (  (kääntö (vasen,0) #+ paikka
          ,kääntö (oikea,0) #+ paikka)
          ,
          (kääntö (vasen,120) #+ paikka
          ,kääntö (oikea,120) #+ paikka)
       )


laskeudu :: Kopteri -> Kopteri
laskeudu kopteri@(Kopteri {kop_nopeus=(_vX,vY)})
    = kopteri {kop_kulma = 0, kop_nopeus = (0,max 0 vY)}

onkoHyväLaskeutuminen :: Kopteri -> Bool
onkoHyväLaskeutuminen kopteri
    =  magV (kop_nopeus kopteri) < 80 && abs (kop_kulma kopteri) <= 10 

kallista :: Float -> Kopteri -> Kopteri
kallista muutos kopteri = kopteri{kop_kulma = muutos + kop_kulma kopteri}

muutaTehoa :: Float -> Kopteri -> Kopteri
muutaTehoa muutos kopteri = kopteri{kop_teho = muutos + kop_teho kopteri}
