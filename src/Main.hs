import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Term
import Entity
import Zone
import Map
import World
import Player

import GameData

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
    rat <- getRat (10,15)
    player <- getStartPlayer
    let zone = Zone {
        _map = map_,
        entities = [rat]
    }
    let world1 = World {
        time = 0.0,
        zone = zone,
        player = player,
        score = 0
    }
    let term = Term ["Welcome to Vent Crawler 2 (No Relation)"] "you@game:~$" "" [] world1
    playIO Fullscreen white 1 term (renderWorld . world) (\e t -> ( ((handleInput e (world t)) >>= (\w -> return t{world = w})))) (tStep)

renderWorld :: Term -> IO Picture
renderWorld t = let w = world t
                    txt = buff t
                    txtPic = [ translate (-360) (-240+20*i) . scale (0.3) (0.3) $ Pictures $ map text txt
                    etts = entities . zone $ w
                    erenders = [draw e (time w) | e <- etts]
                    tilerenders=renderTiles (tiles . _map . zone $ w)
                in return $ Pictures $ (tilerenders:(renderPlayer (player w) (time w)):erenders) ++ [txtPic]
