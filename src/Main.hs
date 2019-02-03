import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Zone
import Map
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
    let map_ = Map {
        tiles = er
    }
    let zone = Zone {
        _map = map_,
        entities = []
    }
    player <- getStartPlayer
    let world = World {
        time = 0.0,
        zone = zone,
        player = player,
        score = 0
    }
    playIO (InWindow "7HRL!" (720,480) (400,400)) white 1 world renderWorld handleInput updateWorld
