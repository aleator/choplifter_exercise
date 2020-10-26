module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Prelude hiding (Down)
import Data.List (partition)

alkutilanne :: PeliTilanne 
alkutilanne =
    GameOn 
      (Peli 
       0 
       alkuKopteri
       [Talo 800 500 700]
       [Hemmo (700, 800), Hemmo (900, 800)]
      )

alkuKopteri :: Kopteri
alkuKopteri = Kopteri
       (10,0) 
       (0,0) 
       0 
       0
       0 -- hemmoa

main :: IO ()
-- main = animate 
--          (InWindow "Choplifter" (400,400) (200,200))
--          (light blue)
--          (flip piirräHemmo (Hemmo (0,0)))
-- 
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
                                            cl{cl_kopteri = (cl_kopteri cl)
                                                                 {kop_kulma = 0
                                                                 , kop_nopeus = 
                                                                            pysäytäPystyssä 
                                                                                (cl_nopeus cl)}})
                            | otherwise -> GameOver cl

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
            (dX,dY) = kulmaJaTehoKiihtyvyydeksi (kop_teho kopteri) (kop_kulma kopteri)
            (kopteriX,kopteriY) = kop_paikka kopteri
            (vX,vY) = kop_nopeus kopteri
            nouseekoKyytiin hemmo = magV (hemmo_sijainti hemmo #- (kopteriX,kopteriY)) < 50
            (hemmotKopteriin,hemmotUlkona) = partition nouseekoKyytiin hemmot
           in Peli (aika + aikaEdellisestä) 
                   (kopteri{
                            kop_paikka = (kopteriX+ aikaEdellisestä *  vX
                                         , max 0 (kopteriY+aikaEdellisestä *  vY) )
                           ,kop_nopeus = ((vX + dX) * 0.97 , (vY + dY - 5) * 0.97 )
                           ,kop_hemmojaKyydissä = (kop_hemmojaKyydissä kopteri 
                                                   + genericLength hemmotKopteriin)
                            })
                   talot
                   (map (päivitäHemmoa edellinenTila) hemmotUlkona)

kulmaJaTehoKiihtyvyydeksi :: Float -> Float -> (Float,Float)
kulmaJaTehoKiihtyvyydeksi teho kulma 
    = rotateV (- degToRad kulma) (0,teho) 

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

data Kopteri = Kopteri {
     kop_paikka :: (Float, Float) -- ^ Missä kopteri?
    ,kop_nopeus :: (Float, Float) -- ^ Kuinka nopeasti menee?
    ,kop_teho   :: Float          -- ^ Teho
    ,kop_kulma  :: Float          -- ^ Kuinka vinossa
    ,kop_hemmojaKyydissä :: Natural -- Kuinka monta hemmoa kerätty 

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


-- Hemmot 
data Hemmo = Hemmo {hemmo_sijainti :: Point}

haluaakoLiikkua :: Choplifter -> Hemmo -> Bool
haluaakoLiikkua peli hemmo = haluaaLiikkua && not putoaako
     where
        kopterinPaikka = cl_paikka peli

        putoaako = abs (korkeusEdessä - snd (hemmo_sijainti hemmo)) > 50
        korkeusEdessä = korkeusKohdassa (fst (hemmo_sijainti hemmo) + suunta * 2)
                                        peli  

        haluaaLiikkua = magV (kopterinPaikka #- hemmo_sijainti hemmo) < 600
        suunta = minneHemmoMenisi peli hemmo

minneHemmoMenisi :: Choplifter -> Hemmo -> Float
minneHemmoMenisi peli hemmo
            | fst kopterinPaikka < fst (hemmo_sijainti hemmo)  
                = -15
            | otherwise             
                =  15
     where
        kopterinPaikka = cl_paikka peli

päivitäHemmoa :: Choplifter -> Hemmo -> Hemmo
päivitäHemmoa peli hemmo 
        | haluaakoLiikkua peli hemmo 
            = hemmo{hemmo_sijainti = hemmo_sijainti hemmo #+ (suunta,0)}
        | otherwise 
            = hemmo
    where   
     suunta = minneHemmoMenisi peli hemmo

piirräHemmo :: Float -> Hemmo -> Picture
piirräHemmo aika hemmo = let 
                     (x,y) = hemmo_sijainti hemmo
                     lantio = (15,40)
                     vasenJalka = 15+sin (12*aika) * 7
                     oikeaJalka = 15+cos (12*aika) * 7
                     hemmonKuva = color white 
                        (translate 0 110 (circleSolid 20)
                          <> line [(0,100), lantio] -- selkä
                          <> line [(-40,90 + cos (8*aika+0.3) * 40),(-30,90), (30,90)
                                  , (40,90 + cos (8*aika) * 40)] -- kädet
                          <> line [(-25,vasenJalka), (-20,vasenJalka) 
                                  , lantio
                                  , (30,oikeaJalka), (35,oikeaJalka)] --jalat
                        )
                    in translate x y hemmonKuva

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

--

(#+) :: Point -> Vector -> Point
(a,b) #+ (x,y) = (a+x,b+y)


(#-) :: Point -> Point -> Vector
(a,b) #- (x,y) = (a-x,b-y)

