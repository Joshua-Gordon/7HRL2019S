import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Zone



main :: IO ()
main = do
    d <- loadBMP "../res/dirt.bmp"
    g <- newStdGen
    let ts = mkTiles g d (30,20)
    let thiccts = expand 5 ts
    let corns = openCorners thiccts d
    let er = erode thiccts d
    let x = 20
    let y = 30
    display (InWindow "7HRL!" (720,480) (400,400)) white (globalTranslate $ renderTiles (getSlice x y er))
