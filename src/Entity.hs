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
draw ent time = let rend = drawDead ent $ drawDamage ent time $ drawRenderer (renderer ent) ent time
                    (ix, iy) = position ent
                    (ilx, ily) = lastPos ent
                    (x, y) = (fromIntegral ix, fromIntegral iy) :: (Float, Float)
                    (lx, ly) = (fromIntegral ilx, fromIntegral ily) :: (Float, Float)
                    u = ((lastMoveTime ent) + moveInterpTime - time) / moveInterpTime
                    rx = if u > 0.0 then u * lx + (1.0 - u) * x else x
                    ry = if u > 0.0 then u * ly + (1.0 - u) * y else y
                in Translate (_TILESIZE * rx) (_TILESIZE * ry) rend

drawDead :: Entity -> Picture -> Picture
drawDead e p = if isDead e then Rotate 180.0 p else p

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
    in if (isDead ent) || phase < baseTime then a else b
drawRenderer (Oriented dirMap dflt) ent time = let
    dir = lastMoveDir ent
    rnd = Map.findWithDefault dflt dir dirMap
    in drawRenderer rnd ent time

data Entity = Entity {
    ent_name :: String, --this should be unique
    position :: (Integer, Integer),
    inventory :: Item.Inv,
    equipped :: Map.Map Slot.Slot (Maybe Item.Stack),
    innateStats :: Stats.Stats,
    renderer :: Renderer,
    lastMoveDir :: Dir.Dir,
	lastPos :: (Integer, Integer),
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
                  

getEmptyEntity :: String -> (Integer,Integer) -> Renderer -> Entity
getEmptyEntity nm pos r = Entity {
    ent_name = nm,
    position = pos,
    inventory = Inv {
                    stacks = Map.empty
                },
    equipped = emptySlots,
    innateStats = baseStats,
    renderer = r,
    lastMoveDir = D,
    lastPos = pos,
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
move e d t = e{lastMoveDir = d, lastMoveTime = t, lastPos = position e, position = Dir.apply d $ position e}

isDead :: Entity -> Bool
isDead e = (curHP e) <= 0

tracePrint :: Show a => String -> a -> a
tracePrint m a = trace (m ++ show a) a

attack :: StdGen -> Entity -> Entity -> Maybe Item.Stack -> Float -> (Entity, Entity, Integer, StdGen)
attack rand attacker defender weapon time = let
    (basetohit_, rand2) = randomR Consts.toHitDie rand
    basetohit = tracePrint "baseToHit: " basetohit_
    (ranged_, improv_) = case weapon of
        Nothing -> (False, True)
        Just stk -> case Item.weaponClass $ Item.item stk of
            Item.Ranged -> (True, False)
            Item.Melee -> (False, False)
            Item.NotAWeapon -> (False, True)
    (ranged, improv) = tracePrint "ranged, improv: " (ranged_, improv_)
    ast = tracePrint "attacker stats: " $ stats attacker
    dst = tracePrint "defender stats: " $ stats defender
    bonus = tracePrint "bonus: " $(if ranged then Stats._DEX ast else Stats._STR ast)
    tohit = tracePrint "toHit: " $ basetohit - (if improv then Consts.improvPenalty else 0) + bonus
    dmgdie = tracePrint "dmgdie: " $ case weapon of
        Nothing -> Consts.improvDamageDie
        Just stk -> Item.damageDie $ Item.item stk
    dmgbon = tracePrint "dmgbon: " $ case weapon of
        Nothing -> Consts.improvDamageBonus
        Just stk -> Item.damageBonus $Item.item stk
    (dmgbase_, rand3) = randomR (1, dmgdie) rand2
    dmgbase = tracePrint "dmgbase: " dmgbase_
    dmg = tracePrint "dmg: " $ dmgbase + dmgbon - Stats._DEF dst
    ac = tracePrint "defender AC: " $ (Stats._AC dst)
    in if tohit < ac || dmg < 0
        then (attacker, defender, 0, rand3)
        else (
            attacker,
            defender{curHP = max ((curHP defender) - dmg) 0, lastDamageTime = time},
            dmg,
            rand3
        )
