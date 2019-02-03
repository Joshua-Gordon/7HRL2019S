module Entity where

import Data.Map as Map
import Debug.Trace
import Data.Maybe
import Data.Fixed
import System.Random

import Graphics.Gloss.Data.Picture
import Graphics.Gloss

import Item
import Slot
import Stats
import Dir
import Consts

data Renderer = Static Picture
    | SimpleCycle Picture Picture
    | Oriented (Map.Map Dir.Dir Renderer) Renderer

draw :: Entity -> Float -> Picture
draw ent time = let rend = drawDamage ent time $ drawRenderer (renderer ent) ent time
                    (x,y) =  position ent
                in Translate (_TILESIZE * fromIntegral x) (_TILESIZE * fromIntegral y) rend

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
    phase = time `mod'` doubleTime
    in if phase < baseTime then a else b
drawRenderer (Oriented dirMap dflt) ent time = let
    dir = lastMoveDir ent
    rnd = Map.findWithDefault dflt dir dirMap
    in drawRenderer rnd ent time

data Entity = Entity {
    position :: (Integer, Integer),
    inventory :: Item.Inv,
    equipped :: Map.Map Slot.Slot (Maybe Item.Stack),
    innateStats :: Stats.Stats,
    renderer :: Renderer,
    lastMoveDir :: Dir.Dir,
    lastMoveTime :: Float,
    lastDamageTime :: Float,
    curHP :: Integer
}

emptySlots :: Map Slot (Maybe Stack)
emptySlots = let cm = singleton ControlModule Nothing
                 hm = insert Helmet Nothing cm
                 ar = insert Armor Nothing hm
                 to = insert Torso Nothing ar
                 aal= insert (ArmAugment Slot.L) Nothing to
                 aar = insert (ArmAugment Slot.R) Nothing aal
                 hal= insert (HandAugment Slot.L) Nothing aar
                 har= insert (HandAugment Slot.R) Nothing hal
                 lal= insert (LegAugment Slot.L) Nothing har
                 lar= insert (LegAugment Slot.R) Nothing lal
                 brl= insert (Bracer Slot.L) Nothing lar
                 brr= insert (Bracer Slot.R) Nothing brl
                 grl= insert (Greave Slot.L) Nothing brr
                 grr= insert (Greave Slot.R) Nothing grl
                 btl= insert (Boot Slot.L) Nothing grr
                 btr= insert (Boot Slot.R) Nothing btl
             in btr
                  

getEmptyEntity :: (Integer,Integer) -> Renderer -> Entity
getEmptyEntity pos r = Entity {
    position = pos,
    inventory = Inv {
                    stacks = Map.empty
                },
    equipped = emptySlots,
    innateStats = baseStats,
    renderer = r,
    lastMoveDir = D,
    lastMoveTime = 0.0,
    lastDamageTime = 0.0,
    curHP = 10 
}

pickup :: Entity -> Stack -> Entity
pickup e s = e{inventory=addStack (inventory e) s}

interpolating :: Float -> Entity -> Bool
interpolating t e = t < (lastMoveTime e) + Consts.moveInterpTime

stats :: Entity -> Stats
stats e = let items = Prelude.map (Item.applyStats . Item.item) $ catMaybes . elems . equipped $ e
          in Prelude.foldl (.) id items $ innateStats e

move :: Entity -> Dir.Dir -> Float -> Entity
move e d t = e{lastMoveDir = d, lastMoveTime = t, position = Dir.apply d $ position e}

isDead :: Entity -> Bool
isDead e = (curHP e) <= 0

attack :: StdGen -> Entity -> Entity -> Maybe Item.Stack -> Float -> (Entity, Entity, StdGen)
attack rand attacker defender weapon time = let
    (basetohit, rand2) = randomR Consts.toHitDie rand
    (ranged, improv) = case weapon of
        Nothing -> (False, True)
        Just stk -> case Item.weaponClass $ Item.item stk of
            Item.Ranged -> (True, False)
            Item.Melee -> (False, False)
            Item.NotAWeapon -> (False, True)
    ast = stats attacker
    dst = stats defender
    bonus = (if ranged then Stats._DEX ast else Stats._STR ast)
    tohit = basetohit - (if improv then Consts.improvPenalty else 0) + bonus
    dmgdie = case weapon of
        Nothing -> Consts.improvDamageDie
        Just stk -> Item.damageDie $ Item.item stk
    dmgbon = case weapon of
        Nothing -> Consts.improvDamageBonus
        Just stk -> Item.damageBonus $Item.item stk
    (dmgbase, rand3) = randomR (1, dmgdie) rand2
    dmg = dmgbase + dmgbon - Stats._DEF dst
    in if tohit < (Stats._AC dst) || dmg < 0
        then (attacker, defender, rand3)
        else (
            attacker,
            defender{curHP = (curHP defender) - dmg, lastDamageTime = time},
            rand3
        )
