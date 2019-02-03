import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Zone

main :: IO ()
main = do
    d <- loadBMP "../res/dirt.bmp"
    g <- newStdGen
    let ts = mkTiles g d (30,20) 
    display (InWindow "7HRL!" (720,480) (400,400)) white (renderTiles ts)
