module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Prelude hiding (Down)

main :: IO ()
main = play 
        (InWindow "Choplifter" (400,400) (200,200))
        (light blue)
        24
        (GameOn 
          (Peli 0 
              (10,0) (0,0) 0 0
              [Talo 800 500 700]
          ))
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
        EventKey (Char 'w') Down _ _ -> muutaTehoa 2.5 peli
        EventKey (Char 's') Down _ _ -> muutaTehoa (-2.5) peli
        EventKey (Char 'a') Down _ _ -> kallista (-8) peli
        EventKey (Char 'd') Down _ _ -> kallista (8) peli
        _ -> peli
    
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
                                            cl{cl_kulma=0
                                              ,cl_nopeus=pysäytäPystyssä (cl_nopeus cl)})
                            | otherwise -> GameOver cl

pysäytäPystyssä :: Vector -> Vector
pysäytäPystyssä (vx,vy) = (vx, max 0 vy)

onkoHyväLaskeutuminen :: Vector -> Float -> Bool
onkoHyväLaskeutuminen nopeus kulma
    | magV nopeus < 5 && abs kulma <= 10 = True
    | otherwise = False


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
          
(#+) :: Point -> Vector -> Point
(a,b) #+ (x,y) = (a+x,b+y)

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

                   kopterikuva = rotate kulma (scale 0.4 0.4 (kopteri teho aika))
                
                   debugViesti = scale 0.5 0.5
                                 (text ((show (magV (cl_nopeus peli))) 
                                        <> "  " 
                                        <> show (ceiling kulma::Int)))
                   
                   peliKuva = translate kopteriX kopteriY kopterikuva 
                                        <> maa  
                                        <> pictures (map piirräTalo talot)
                                        <> apuviivaAla
                                        <> apuviivaYlä
                                        <> debugViesti
                  in scale 0.25 0.25 (translate 0 (-180) peliKuva)

kallista :: Float -> Choplifter -> Choplifter
kallista muutos peli = peli{cl_kulma = muutos + cl_kulma peli}

muutaTehoa :: Float -> Choplifter -> Choplifter
muutaTehoa muutos peli = peli{cl_teho = muutos + cl_teho peli}
                                          ---       ↑
                                          --     cl_teho :: Choplifter -> Float
                          

data PeliTilanne = GameOver Choplifter | GameOn Choplifter

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


