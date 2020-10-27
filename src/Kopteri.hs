module Kopteri where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Aritmetiikka

data Kopteri = Kopteri {
     kop_paikka :: (Float, Float) -- ^ Missä kopteri?
    ,kop_nopeus :: (Float, Float) -- ^ Kuinka nopeasti menee?
    ,kop_teho   :: Float          -- ^ Teho
    ,kop_kulma  :: Float          -- ^ Kuinka vinossa
    ,kop_hemmojaKyydissä :: Natural -- Kuinka monta hemmoa kerätty 
    }

data TörmäysKohta = Laskuteline | Roottori 
        deriving (Eq,Ord,Show)

luoKopteri :: Point -> Kopteri
luoKopteri paikka 
    = Kopteri 
       paikka 
       (0,0) 
       0 
       0
       0 -- hemmoa

päivitäKopteria :: Float -> Natural -> Kopteri -> Kopteri
päivitäKopteria aikaEdellisestä lisääHemmoja kopteri 
 = kopteri{
          kop_paikka = (kopteriX + aikaEdellisestä * vX
                       , max 0 (kopteriY+aikaEdellisestä *  vY) )
          ,kop_nopeus = ((vX + dX) * 0.97 , (vY + dY - 5) * 0.97 )
          ,kop_hemmojaKyydissä = (kop_hemmojaKyydissä kopteri + lisääHemmoja)
          }
 where
    (kopteriX,kopteriY) = kop_paikka kopteri
    (vX,vY) = kop_nopeus kopteri
    (dX,dY) = kulmaJaTehoKiihtyvyydeksi (kop_teho kopteri) (kop_kulma kopteri)


laskeudu :: Kopteri -> Kopteri
laskeudu kopteri = kopteri{ kop_kulma  = 0
                          , kop_nopeus = pysäytäPystyssä (kop_nopeus kopteri)}

onkoHyväLaskeutuminen :: Kopteri -> Bool
onkoHyväLaskeutuminen Kopteri{kop_nopeus=nopeus , kop_kulma=kulma}
    | magV nopeus < 80 && abs kulma <= 10 = True
    | otherwise = False

kulmaJaTehoKiihtyvyydeksi :: Float -> Float -> (Float,Float)
kulmaJaTehoKiihtyvyydeksi teho kulma 
    = rotateV (- degToRad kulma) (0,teho) 

kopteriTörmäysviivat :: Kopteri -> ((Point,Point) , (Point,Point))
kopteriTörmäysviivat kopteri = 
    let
     paikka = kop_paikka kopteri
     kulma = kop_kulma kopteri
     vasen = -170
     oikea = 100 
     kääntö = rotateV (- degToRad kulma)
    in (  (kääntö (vasen,0) #+ paikka
          ,kääntö (oikea,0) #+ paikka)
          ,
          (kääntö (vasen,120) #+ paikka
          ,kääntö (oikea,120) #+ paikka)
       )

kallista :: Float -> Kopteri -> Kopteri
kallista muutos kopteri = kopteri{kop_kulma = muutos + kop_kulma kopteri}

muutaTehoa :: Float -> Kopteri -> Kopteri
muutaTehoa muutos kopteri = kopteri{kop_teho = muutos + kop_teho kopteri}

piirräKopteri :: Float -> Kopteri -> Picture
piirräKopteri aika Kopteri{kop_teho = teho, kop_kulma = kulma, kop_paikka = (kopteriX,kopteriY)} 
    = translate kopteriX kopteriY 
      . rotate kulma 
      . scale 0.4 0.4 
      . translate 0 (150) 
      . color white
      $ runko
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
