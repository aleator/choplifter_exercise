module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Prelude hiding (Down)
import Data.List (partition)

import Aritmetiikka
import Hemmot
import Kopteri

alkutilanne :: PeliTilanne 
alkutilanne =
    GameOn 
      (Peli 
       0 
       (luoKopteri (0,0))
       [Talo 800 500 700]
       [Hemmo (700, 800), Hemmo (900, 800)]
      )

main :: IO ()

main = play 
        (InWindow "Choplifter" (400,400) (200,200))
        (light blue)
        24
        alkutilanne 
        piirräPeliTilanne
        reagoiPeliTilanne
        päivitäPelitilanne

reagoiPeliTilanne :: Event -> PeliTilanne -> PeliTilanne
reagoiPeliTilanne tapahtuma pelitilanne 
    = case pelitilanne of
        GameOver cl -> GameOver cl
        GameOn cl -> GameOn (reagoi tapahtuma cl)

reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli 
    = case tapahtuma of
        EventKey (Char 'w') Down _ _ -> kopterille (muutaTehoa 2.5) peli
        EventKey (Char 's') Down _ _ -> kopterille (muutaTehoa (-2.5)) peli
        EventKey (Char 'a') Down _ _ -> kopterille (kallista (-8)) peli
        EventKey (Char 'd') Down _ _ -> kopterille (kallista (8)) peli
        _ -> peli

kopterille :: (Kopteri -> Kopteri) -> Choplifter -> Choplifter
kopterille f peli = peli{cl_kopteri = f (cl_kopteri peli)} 

cl_paikka :: Choplifter -> Point
cl_paikka = kop_paikka . cl_kopteri

cl_nopeus :: Choplifter -> Vector
cl_nopeus = kop_nopeus . cl_kopteri

cl_kulma, cl_teho :: Choplifter -> Float
cl_kulma = kop_kulma . cl_kopteri
cl_teho  = kop_teho . cl_kopteri
    
päivitäPelitilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPelitilanne aikaEdellisestä pelitilanne 
    = case pelitilanne of
        GameOver cl -> GameOver cl
        GameOn cl   -> case  törmääköTaloon (cl_paikka cl) (cl_kulma cl) (cl_talot cl) of
                        Nothing -> GameOn (päivitäPeliä aikaEdellisestä cl)
                        Just Roottori -> GameOver cl
                        Just Laskuteline 
                            | onkoHyväLaskeutuminen (cl_nopeus cl) (cl_kulma cl)
                                -> GameOn (päivitäPeliä aikaEdellisestä 
                                            (kopterille laskeudu cl))
                            | otherwise -> GameOver cl

laskeudu :: Kopteri -> Kopteri
laskeudu kopteri@(Kopteri {kop_nopeus=(_vX,vY)})
    = kopteri {kop_kulma = 0, kop_nopeus = (0,max 0 vY)}

pysäytäPystyssä :: Vector -> Vector
pysäytäPystyssä (vx,vy) = (vx, max 0 vy)

onkoHyväLaskeutuminen :: Vector -> Float -> Bool
onkoHyväLaskeutuminen nopeus kulma
    | magV nopeus < 80 && abs kulma <= 10 = True
    | otherwise = False


päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila 
  = case edellinenTila of
     Peli aika kopteri talot hemmot
        -> let
            nouseekoKyytiin hemmo = magV (hemmo_sijainti hemmo #- kop_paikka kopteri) < 50
            (hemmotKopteriin,hemmotUlkona) = partition nouseekoKyytiin hemmot
           in Peli (aika + aikaEdellisestä) 
                   (noukiHemmot hemmotKopteriin . päivitäKopteri aikaEdellisestä $ kopteri)
                   talot
                   (map (päivitäHemmoa (flip korkeusKohdassa edellinenTila) (kop_paikka kopteri)) 
                        hemmotUlkona)

noukiHemmot :: [Hemmo] -> Kopteri -> Kopteri
noukiHemmot hemmot kopteri 
    = kopteri{
       kop_hemmojaKyydissä = (kop_hemmojaKyydissä kopteri + genericLength hemmot)
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

kulmaJaTehoKiihtyvyydeksi :: Float -> Float -> (Float,Float)
kulmaJaTehoKiihtyvyydeksi teho kulma 
    = rotateV (- degToRad kulma) (0,teho) 


data TörmäysKohta = Laskuteline | Roottori 
        deriving (Eq,Ord,Show)

törmääköTaloon :: Point -> Float -> [Talo] -> Maybe TörmäysKohta
törmääköTaloon paikka kulma talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
                                   -- case (nonEmpty (mapMaybe törmääköYhteen talot)) of
                                   --  Nothing -> Nothing
                                   --  Just kohdat -> Just (maximum1 kohdat)
    where
     törmääköYhteen talo 
        = let 
            ((ala1,ala2),(ylä1,ylä2)) = kopteriTörmäysviivat paikka kulma
            (va,oy)   = nurkkaPisteet talo 
          in case (not (segClearsBox ala1 ala2 va oy), not (segClearsBox ylä1 ylä2 va oy)) of
                (True,False) -> Just Laskuteline
                (False,False) -> Nothing
                _ -> Just Roottori
          

piirräPeliTilanne :: PeliTilanne -> Picture
piirräPeliTilanne pelitilanne 
    = case pelitilanne of
        GameOver cl -> piirräPeli cl <> translate (-300) 0 (color yellow (text "GAME OVER"))
        GameOn cl   -> piirräPeli cl

piirräPeli :: Choplifter -> Picture
piirräPeli peli = let
                   kulma = cl_kulma peli 
                   aika  = cl_aika peli
                   (kopteriX,kopteriY) = cl_paikka peli
                   teho = cl_teho peli
                   talot = cl_talot peli

                   ((va,oa), (va1,oa1)) = kopteriTörmäysviivat (kopteriX, kopteriY) kulma
                   apuviivaAla = color red (line [va,oa])
                   apuviivaYlä= color red (line [va1,oa1])

                   kopterikuva = rotate kulma (scale 0.4 0.4 (piirräKopteri teho aika))

                   hemmoKuvat = map (piirräHemmo aika)  (cl_hemmot peli)
                   taloKuvat  = map piirräTalo talot
                   peliKuva = translate kopteriX kopteriY kopterikuva 
                                        <> maa  
                                        <> pictures taloKuvat
                                        <> pictures hemmoKuvat
                                        <> apuviivaAla
                                        <> apuviivaYlä
                                        
                  in scale 0.25 0.25 (translate 0 (-180) peliKuva)

kallista :: Float -> Kopteri -> Kopteri
kallista muutos kopteri = kopteri{kop_kulma = muutos + kop_kulma kopteri}

muutaTehoa :: Float -> Kopteri -> Kopteri
muutaTehoa muutos kopteri = kopteri{kop_teho = muutos + kop_teho kopteri}
                          

data PeliTilanne = GameOver Choplifter | GameOn Choplifter

data Choplifter 
 = Peli 
   {
     cl_aika   :: Float          -- ^ Aika pelin alusta

    ,cl_kopteri :: Kopteri       -- kopterin tiedot
   
    ,cl_talot  :: [Talo]         -- Esteet pelissä
    ,cl_hemmot :: [Hemmo]        -- Pelihahmot
    
   }



korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli =
  maybe 0 maximum1 . nonEmpty . map osuukoTaloon . cl_talot $ peli
 where
  osuukoTaloon :: Talo -> Float
  osuukoTaloon talo
    | abs (talo_sijainti talo - kohta) < (talo_leveys talo / 2) = talo_korkeus
      talo
    | otherwise = 0



--- Talot
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

-- type Point = (Float,Float)
nurkkaPisteet :: Talo -> (Point,Point)
nurkkaPisteet talo = 
    let
        vasenAla = (talo_sijainti talo - (talo_leveys talo / 2) , 0)
        oikeaYlä = (talo_sijainti talo + (talo_leveys talo / 2)      , talo_korkeus talo) 
    in (vasenAla,oikeaYlä)

maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))


--

