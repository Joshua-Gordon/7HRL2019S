module Player where

import Debug.Trace

import Graphics.Gloss

import Data.Map
import Entity
import Dir

data Player = Player {
    name :: String,
    entity :: Entity
}

getStartPlayer :: IO Player
getStartPlayer = do
                   bl <- loadBMP "../res/player0backL.bmp" 
                   br <- loadBMP "../res/player0backR.bmp"
                   let back = SimpleCycle bl br
                   fl <- loadBMP "../res/player0frontL.bmp" 
                   fr <- loadBMP "../res/player0frontR.bmp" 
                   let front = SimpleCycle fl fr
                   ll <- loadBMP "../res/player0leftL.bmp" 
                   lr <- loadBMP "../res/player0leftR.bmp" 
                   let left = SimpleCycle ll lr
                   rl <- loadBMP "../res/player0rightL.bmp" 
                   rr <- loadBMP "../res/player0rightR.bmp"
                   let right = SimpleCycle rl rr
                   let renderMap = insert D front $ insert L left $ insert R right $ singleton U back
                   let renderer = Oriented renderMap front
                   let ent = getEmptyEntity "bobert" (0,0) renderer
                   return Player {
                       name = "bobert",
                       entity=ent
                   }

renderPlayer :: Player -> Float -> Picture
renderPlayer p f = drawDamage (entity p) f $ drawRenderer (renderer . entity $ p) (entity p) f
