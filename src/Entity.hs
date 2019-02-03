module Entity where

import Data.Map as Map
import System.Random

import Graphics.Gloss.Data.Picture

import Item
import Slot
import Stats
import Dir
import Consts

data Renderer = Static Picture
	| SimpleCycle Picture Picture
	| Oriented Map.Map Dir.Dir Renderer Renderer

draw :: Entity -> Float -> Picture
draw ent time = drawDamage ent time $ drawRenderer (renderer ent) ent time

drawDamage :: Entity -> Float -> Picture -> Picture
drawDamage ent time rend = let
	dt = (lastDamageTime ent) + Consts.damageAnimTime - time
	in if dt > 0
		then Rotate (Consts.damageAnimIntensity * dt) rend
		else rend

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
	lastDamageTime :: Float,
	curHP :: Integer
}

interpolating :: Float -> Entity -> Bool
interpolating t e = t < (lastMoveTime e) + Consts.moveInterpTime

stats :: Entity -> Stats
stats e = foldl ($) (innateStats e) $ filter (/= Nothing) $ elems $ equipped e

move :: Entity -> Dir.Dir -> Float -> Entity
move e d t = e{lastMoveDir = d, lastMoveTime = t, position = Dir.apply $ position e}

isDead :: Entity -> Bool
isDead e = (curHP e) <= 0

attack :: StdGen -> Entity -> Entity -> Maybe Item.Stack -> Float -> (Entity, Entity, StdGen)
attack rand attacker defender weapon time = let
	(basetohit, rand2) = randomR Consts.toHitDie rand
	(ranged, improv) = case weapon of
		Nothing -> (False, True)
		Just stk -> case Item.weaponClass $ Item.item stk of
			Item.WeaponClass.Ranged -> (True, False)
			Item.WeaponClass.Melee -> (False, False)
			Item.WeaponClass.NotAWeapon -> (False, True)
	ast = stats attacker
	dst = stats defender
	bonus = (if ranged then Stats.DEX ast else Stats.STR ast)
	tohit = basetohit - (if improv then Consts.improvPenalty else 0) + bonus
	dmgdie = case weapon of
		Nothing -> Consts.improvDamageDie
		Just stk -> Item.damageDie $ Item.item stk
	dmgbon = case weapon of
		Nothing -> Consts.improvDamageBonus
		Just stk -> Item.damageBonus $Item.item stk
	(dmgbase, rand3) = randomR (1, dmgdie) rand2
	dmg = dmgbase + dmgbon - Stats.DEF dst
	in if tohit < (Stats.AC dst) || dmg < 0
		then (attacker, defender, rand3)
		else (
			attacker,
			defender{curHP = (curHP defender) - dmg, lastDamageTime = time},
			rand3
		)
