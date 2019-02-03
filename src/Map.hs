module Map where

import Graphics.Gloss

data Tile = Floor Picture | Wall deriving Eq

data Map = Map {
    tiles :: [[Tile]]
}

isFloor :: Tile -> Bool
isFloor (Floor _) = True
isFloor _ = False
