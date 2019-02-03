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

globalTranslate :: World -> Picture -> Picture
globalTranslate w p = let pl = player w
                          (x,y) = position . entity $ pl
                          dx = (*) _TILESIZE $ fromIntegral x - (fromIntegral _WIDTH)/2
                          dy = (*) _TILESIZE $ fromIntegral y - (fromIntegral _HEIGHT)/2
                      in Translate dx dy p


updateWorld :: Float -> World -> IO World
updateWorld f w = let t =  time w
                  in return w{time=t+f}

tryMove :: World -> Dir -> Entity -> Entity
tryMove w d e = if isFloor $ (tiles . _map . zone $ w) !! (fromIntegral x) !! (fromIntegral y) then e2 else e
  where
    e2 = move e  d (time w) 
    (x,y) = position e2



