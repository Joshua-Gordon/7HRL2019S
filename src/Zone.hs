module Zone where

import Graphics.Gloss
import System.Random

import Entity
import Consts
import Map


data Zone = Zone {
    _map :: Map,
    entities :: [Entity]
}

mkTiles :: StdGen -> Picture -> (Int,Int) -> [[Tile]]
mkTiles g ft (w,h) = let rs = randoms g :: [Bool]
                  in  runCells 50 [[if rs !! (j*w+i) then Floor ft else Wall | i <- [0..h-1]] | j <- [0..w-1]] ft

runCells :: Int -> [[Tile]] -> Picture -> [[Tile]]
runCells n ts ft | n == 0 = ts
                 | otherwise = runCells (n-1) [[step x y ts ft | x <- [0..length ts -1]] | y <- [0..length (head ts) -1]] ft

step :: Int -> Int -> [[Tile]] -> Picture -> Tile
step x y ts ft = let n = neighbors x y ts
              in if n == 3 then Floor ft else if n == 0 then Wall else if n < 6 then ts !!x!!y else Wall

neighbors :: Int -> Int -> [[Tile]] -> Int
neighbors x y ts = let range = [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
                       ns = filter (/=Nothing) [Just ts !!! xr !!! yr | (xr,yr) <- range]
                   in length $ filter (/=(Just Wall)) ns

(!!!) :: Maybe [a] -> Int -> Maybe a
Nothing !!! _ = Nothing
(Just (x:xs)) !!! n | n == 0 = Just x
                    | null xs = Nothing
                    | otherwise = (Just xs) !!! (n-1)

renderTile :: Tile -> Float -> Float -> Picture
renderTile Wall x y = let c = Color black sq
                      in Translate x y c
renderTile (Floor p) x y = Translate x y p

renderTiles :: [[Tile]] -> Picture
renderTiles ts = let w = length ts
                     h = length (head ts)
                     pics = [[ renderTile (ts!!col!!row) (_TILESIZE * fromIntegral col) (_TILESIZE * fromIntegral row) | row <- [0..h-1] ] | col <- [0..w-1]]
                 in  Pictures $ Prelude.map Pictures pics

expand :: Int -> [[Tile]] -> [[Tile]]
expand factor ts = let w = length ts
                       h = length (head ts)
                   in [[ts !! (x `div` factor) !! (y `div` factor) | y <- [0..factor*h - 1]] | x <- [0..factor*w - 1]]

erode :: [[Tile]] -> Picture -> [[Tile]]
erode ts ft = let w = length ts
                  h = length (head ts)
              in [[ if neighbors x y ts > 3 then Floor ft else ts!!x!!y | y <- [0..h-1]] | x <- [0..w-1]]

openCorners :: [[Tile]] -> Picture -> [[Tile]]
openCorners ts ft = let w = length ts
                        h = length (head ts)
                    in [[ if neighbors x y ts == 3 then Floor ft else ts!!x!!y | y <- [0..h-1]] | x <- [0..w-1]]

getSlice :: Int -> Int -> [[Tile]] -> [[Tile]]
getSlice x y = drop x . map (drop y) 
