import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Term
import Zone
import World
import Player


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
    let zone = Zone {
        tiles = er,
        entities = []
    }
    let player = Player "bbrian"
    let world1 = World {
        time = 0.0,
        zone = zone,
        player = player,
        score = 0
    }
    let term = Term ["Welcome to Vent Crawler 2 (No Relation)"] "you@game:~$" "" [] world1
    playIO (InWindow "7HRL!" (720,480) (400,400)) white 1 term (renderWorld . world) (\e t -> ( ((handleInput e (world t)) >>= (\w -> return t{world = w})))) (tStep)

