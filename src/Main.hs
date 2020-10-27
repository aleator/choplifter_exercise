module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Geometry.Line
import Prelude hiding (Down)
import Data.List (partition)
import System.Random

import Aritmetiikka
import Talot
import Hemmot
import Kopteri

alkutilanne :: Float 
                -- ^ satunnaisluku
                -> PeliTilanne 
alkutilanne 
 = GameOn 
      (Peli 
       0 
       (luoKopteri (0,0))
       [Talo 800 500 700]
       [Hemmo (700, 800), Hemmo (900, 800)]
      )

main :: IO ()
-- main = animate 
--          (InWindow "Choplifter" (400,400) (200,200))
--          (light blue)
--          (flip piirräHemmo (Hemmo (0,0)))
-- 
main 
 = do
      --koko <- getScreenSize 
      satunnaisluku <- randomRIO (0,10)
      play 
        (InWindow "Choplifter" (400,400) (200,200))
        (light blue)
        24
        (alkutilanne satunnaisluku)
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
    
päivitäPelitilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPelitilanne aikaEdellisestä pelitilanne 
    = case pelitilanne of
        GameOver peli -> GameOver peli
        GameOn peli   -> case  törmääköTaloon (kopteriTörmäysviivat (cl_kopteri peli)) (cl_talot peli) of
                        Nothing -> GameOn (päivitäPeliä aikaEdellisestä peli)
                        Just Roottori -> GameOver peli
                        Just Laskuteline 
                            | onkoHyväLaskeutuminen (cl_kopteri peli) 
                                -> GameOn (päivitäPeliä aikaEdellisestä (kopterille laskeudu peli))
                            | otherwise -> GameOver peli

kopterille :: (Kopteri -> Kopteri) -> Choplifter -> Choplifter
kopterille f peli = peli{cl_kopteri = f (cl_kopteri peli)}

päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila 
  = case edellinenTila of
     Peli aika kopteri talot hemmot
        -> let
            paikka = kop_paikka kopteri
            nouseekoKyytiin hemmo = magV (hemmo_sijainti hemmo #- paikka) < 50
            (hemmotKopteriin,hemmotUlkona) = partition nouseekoKyytiin hemmot
           in Peli (aika + aikaEdellisestä) 
                   (päivitäKopteria aikaEdellisestä (genericLength hemmotKopteriin) kopteri) 
                   talot
                   (map (päivitäHemmoa (flip korkeusKohdassa edellinenTila) 
                                       paikka) hemmotUlkona)


törmääköTaloon :: ((Point,Point),(Point,Point)) -> [Talo] -> Maybe TörmäysKohta
törmääköTaloon törmäysviivat talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
                                        -- ↑           ↑
                                        --            [a] -> Maybe (NonEmpty a)
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
                   aika  = cl_aika peli
                   talot = cl_talot peli

                   kopterikuva =  piirräKopteri aika (cl_kopteri peli)

                   hemmoKuvat = map (piirräHemmo aika)  (cl_hemmot peli)
                   taloKuvat  = map piirräTalo talot
                   peliKuva = kopterikuva 
                               <> maa  
                               <> pictures taloKuvat
                               <> pictures hemmoKuvat
                                        
                  in scale 0.25 0.25 (translate 0 (-180) peliKuva)

                                          ---       ↑
                                          --     cl_teho :: Choplifter -> Float

data PeliTilanne = GameOver Choplifter | GameOn Choplifter

data Choplifter 
 = Peli 
   {
     cl_aika   :: Float          -- ^ Aika pelin alusta
    ,cl_kopteri :: Kopteri  
    ,cl_talot  :: [Talo]         -- Esteet pelissä
    ,cl_hemmot :: [Hemmo]        -- Pelihahmot
   }



korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli =
  maybe 0 maximum1 . nonEmpty . map (osuukoTaloon kohta) . cl_talot $ peli

-- Hemmot 


-- type Point = (Float,Float)

maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))


