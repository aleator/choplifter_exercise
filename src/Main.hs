module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Prelude hiding (Down)
import Data.List (partition)

import Aritmetiikka
import Hemmot
import Kopteri
import Talot

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

päivitäPelitilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPelitilanne aikaEdellisestä pelitilanne 
    = case pelitilanne of
        GameOver cl -> GameOver cl
        GameOn cl   -> case  törmääköTaloon (kopteriTörmäysviivat (cl_kopteri cl)) (cl_talot cl)  of
                        Nothing -> GameOn (päivitäPeliä aikaEdellisestä cl)
                        Just Roottori -> GameOver cl
                        Just Laskuteline 
                            | onkoHyväLaskeutuminen (cl_kopteri cl)
                                -> GameOn (päivitäPeliä aikaEdellisestä 
                                            (kopterille laskeudu cl))
                            | otherwise -> GameOver cl


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

data TörmäysKohta = Laskuteline | Roottori 
        deriving (Eq,Ord,Show)

törmääköTaloon :: ((Point,Point),(Point,Point)) -> [Talo] -> Maybe TörmäysKohta
törmääköTaloon törmäysviivat talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
                                   -- case (nonEmpty (mapMaybe törmääköYhteen talot)) of
                                   --  Nothing -> Nothing
                                   --  Just kohdat -> Just (maximum1 kohdat)
    where
     törmääköYhteen talo 
        = let 
            ((ala1,ala2),(ylä1,ylä2)) = törmäysviivat
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
                   talot = cl_talot peli

                   kopterikuva = piirräKopteri (cl_aika peli) (cl_kopteri peli)

                   hemmoKuvat = map (piirräHemmo (cl_aika peli))  (cl_hemmot peli)
                   taloKuvat  = map piirräTalo talot
                   peliKuva = maa  
                              <> pictures taloKuvat
                              <> pictures hemmoKuvat
                              <> kopterikuva
                                        
                  in scale 0.25 0.25 (translate 0 (-180) peliKuva)


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
  maybe 0 maximum1 . nonEmpty . map (osuukoTaloon kohta) . cl_talot $ peli



maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))


--

