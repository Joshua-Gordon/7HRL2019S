module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Zone
import Dir
import Map
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
globalTranslate p = Translate ((fromIntegral $ -_WIDTH)/2) ((fromIntegral $ -_HEIGHT)/2) p

renderWorld :: World -> IO Picture
renderWorld w = let --etts = entities . zone $ w
                    --erenders = [draw e (time w) | e <- etts]
                    --tilerenders=renderTiles (tiles . _map . zone $ w)
                in return (renderPlayer (player w) (time w))
                --in return $ Pictures $ tilerenders:(renderPlayer (player w) (time w)):erenders

handleInput :: Event -> World -> IO World
handleInput e = return

updateWorld :: Float -> World -> IO World
updateWorld f w = let t =  time w
                  in return w{time=t+f}

tryMove :: World -> Dir -> Entity -> Entity
tryMove w d e = if isFloor $ (tiles . _map . zone $ w) !! (fromIntegral x) !! (fromIntegral y) then e2 else e
  where
    e2 = move e  d 1 
    (x,y) = position e2



