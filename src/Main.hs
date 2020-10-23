module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Prelude hiding (Down)

main :: IO ()
main = play 
        (InWindow "Choplifter" (400,400) (200,200))
        (light blue)
        24
        (Peli 0 
              (10,0) (0,0) 0 0
              [Talo 800 500 700]
        )
        piirräPeli
        reagoi
        päivitäPeliä

reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli 
    = case tapahtuma of
        EventKey (Char 'w') Down _ _ -> muutaTehoa 2.5 peli
        EventKey (Char 's') Down _ _ -> muutaTehoa (-2.5) peli
        EventKey (Char 'a') Down _ _ -> kallista (-8) peli
        EventKey (Char 'd') Down _ _ -> kallista (8) peli
        _ -> peli
    

päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila 
  = case edellinenTila of
     Peli aika (kopteriX,kopteriY) 
               (vX,vY) 
               teho kulma
               talot
        -> let
            (dX,dY) = kulmaJaTehoKiihtyvyydeksi teho kulma
           in Peli (aika + aikaEdellisestä) 
                   (kopteriX+ aikaEdellisestä *  vX
                   , max 0 (kopteriY+aikaEdellisestä *  vY) )
                   ((vX + dX) * 0.97 , (vY + dY - 5) * 0.97 )
                   teho
                   kulma
                   talot

kulmaJaTehoKiihtyvyydeksi :: Float -> Float -> (Float,Float)
kulmaJaTehoKiihtyvyydeksi teho kulma 
    = rotateV (- degToRad kulma) (0,teho) 

piirräPeli :: Choplifter -> Picture
piirräPeli peli = let
                   kulma = cl_kulma peli 
                   aika  = cl_aika peli
                   (kopteriX,kopteriY) = cl_paikka peli
                   teho = cl_teho peli
                   talot = cl_talot peli

                   kopterikuva = rotate kulma (scale 0.4 0.4 (kopteri teho aika))
                   peliKuva = translate kopteriX kopteriY kopterikuva
                                        <> maa  
                                        <> pictures (map piirräTalo talot)
                  in scale 0.25 0.25 (translate 0 (-180) peliKuva)

kallista :: Float -> Choplifter -> Choplifter
kallista muutos peli = peli{cl_kulma = muutos + cl_kulma peli}

muutaTehoa :: Float -> Choplifter -> Choplifter
muutaTehoa muutos peli = peli{cl_teho = muutos + cl_teho peli}
                                          ---       ↑
                                          --     cl_teho :: Choplifter -> Float
                          

data Choplifter 
 = Peli 
   {
     cl_aika   :: Float          -- ^ Aika pelin alusta

    ,cl_paikka :: (Float, Float) -- ^ Missä kopteri?
    ,cl_nopeus :: (Float, Float) -- ^ Kuinka nopeasti menee?
    ,cl_teho   :: Float          -- ^ Teho
    ,cl_kulma  :: Float          -- ^ Kuinka vinossa
   
    ,cl_talot  :: [Talo]         -- Esteet pelissä
    
   }

data Talo = Talo {talo_korkeus :: Float, talo_leveys :: Float
                 ,talo_sijainti :: Float }

--- Tästä alaspäin piirtofunktioita

piirräTalo :: Talo -> Picture
piirräTalo talo = let
                   paikoillaan = translate (talo_sijainti talo) (talo_korkeus talo / 2) talonKuva
                   talonKuva = color (greyN 0.5) 
                                (rectangleSolid (talo_leveys talo) (talo_korkeus talo))
                  in paikoillaan

maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))

kopteri :: Float -> Float -> Picture
kopteri teho aika = translate 0 (150) (color white runko)
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


