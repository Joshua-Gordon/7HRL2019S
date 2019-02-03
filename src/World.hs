module World where

import Zone
import Player
import Consts
import Entity

data World = World { --ZA WARUDO
    time :: Float,   --STOP TIME
    zone :: Zone,
    player :: Player,
    score :: Integer
}

globalTranslate :: Picture -> Picture
globalTranslate p = Translate (-_WIDTH/2) (-_HEIGHT/2) p

renderWorld :: World -> IO Picture
renderWorld w = let etts = entities . zone $ w
                    erenders = [draw e (time w) | e <- etts]
                in globalTranslate $ Pictures [erenders, renderTiles (tiles . zone $ w)]

handleInput :: Event -> World -> IO World
handleInput e = return

updateWorld :: Float -> World -> IO World
updateWorld f w = let t =  time w
                  in return w{time=t+f}
