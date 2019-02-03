module Entity where

import Data.Map as Map

import Item
import Slot
import Stats
import Dir
import Consts

data Renderer = Static Picture
	| SimpleCycle Picture Picture
	| Oriented Map.Map Dir.Dir Renderer Renderer

draw :: Entity -> Float -> Picture
draw ent time = drawRenderer (renderer ent) ent time

drawRenderer (Static p) ent time = p
drawRenderer (SimpleCycle a b) ent time = let
	baseTime = if (interpolating time ent) then movingWalkCycleTime else walkCycleTime
	doubleTime = 2.0 * baseTime
	phase = time `mod` doubleTime
	in if phase < baseTime then a else b
drawRenderer (Oriented dirMap dflt) ent time = let
	dir = lastMoveDir ent
	rnd = Map.findWithDefault dflt dir dirMap
	in drawRenderer rnd ent time

data Entity = Entity {
	position :: (Integer, Integer),
	inventory :: Item.Inv,
	equipped :: Map.Map Slot.Slot Maybe Item.Stack,
	innateStats :: Stats.Stats,
	renderer :: Renderer,
	lastMoveDir :: Dir.Dir,
	lastMoveTime :: Float,
}

interpolating :: Float -> Entity -> Bool
interpolating t e = t < (lastMoveTime e) + Consts.moveInterpTime

stats :: Entity -> Stats
stats e = foldl ($) (innateStats e) $ filter (/= Nothing) $ elems $ equipped e

move :: Entity -> Dir.Dir -> Float -> Entity
move e d t = e{lastMoveDir = d, lastMoveTime = t, position = Dir.apply $ position e}
