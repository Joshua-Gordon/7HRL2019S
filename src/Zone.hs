module Zone where

import Graphics.Gloss
import System.Random

_TILESIZE :: Float
_TILESIZE = fromIntegral 24

sq :: Picture
sq = Polygon [(-_TILESIZE/2,-_TILESIZE/2),(_TILESIZE/2,-_TILESIZE/2),(_TILESIZE/2,_TILESIZE/2),(-_TILESIZE/2,_TILESIZE/2)]

data Tile = Floor Picture | Wall

data Zone = Zone {
    tiles :: [[Tile]],
    --entity list
    score :: Integer
}

mkTiles :: StdGen -> Picture -> (Int,Int) -> [[Tile]]
mkTiles g ft (w,h) = let rs = randoms g :: [Bool]
                  in  [[if rs !! (j*w+i) then Floor ft else Wall | i <- [0..h-1]] | j <- [0..w-1]]

renderTile :: Tile -> Float -> Float -> Picture
renderTile Wall x y = let c = Color black sq
                      in Translate x y c
renderTile (Floor p) x y = Translate x y p

renderTiles :: [[Tile]] -> Picture
renderTiles ts = let w = length ts
                     h = length (head ts)
                     pics = [[ renderTile (ts!!col!!row) (_TILESIZE * fromIntegral col) (_TILESIZE * fromIntegral row) | row <- [0..h-1] ] | col <- [0..w-1]]
                 in  Pictures $ map Pictures pics
