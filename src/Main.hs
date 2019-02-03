import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Zone

_WIDTH = 720
_HEIGHT = 480

globalTranslate :: Picture -> Picture
globalTranslate p = Translate (-_WIDTH/2) (-_HEIGHT/2) p

main :: IO ()
main = do
    d <- loadBMP "../res/dirt.bmp"
    g <- newStdGen
    let ts = mkTiles g d (30,20) 
    display (InWindow "7HRL!" (720,480) (400,400)) white (globalTranslate $ renderTiles ts)
