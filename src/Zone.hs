module Zone where

import System.Random

data Tile = Floor | Wall

data Zone = Zone {
    tiles :: [[Tile]],
    --entity list
    score :: Integer
}

getRandomTile :: StdGen -> (Tile,StdGen)
getRandomTile g = let b = random --

mkTiles :: StdGen -> [[Tile]]
mkTiles g = let b = random g :: Bool
                piece = if b then Floor else Wall

