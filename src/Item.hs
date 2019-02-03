module Item where

import Data.Map as Map

import Stats (Stats)
import Slot (Slot)

data WeaponClass = NotAWeapon | Melee | Ranged

data Item = Item {
	name :: String,
	applyStats :: Stats -> Stats,
	slotting :: [Slot],
	weaponClass :: WeaponClass,
	damageDie :: Integer,
	damageBonus :: Integer,
}

data Stack = Stack {
	item :: Item,
	amount :: Integer,
}

combine :: Stack -> Stack -> Stack
combine s1 s2 = s1{amount = (amount s1) + (amount s2)}

data Inv = Inv {
	stacks :: Map.Map Item Stack,
}

addStack :: Inv -> Stack -> Inv
addStack i s = i{stacks = Map.insertWith combine (item s) s (stacks i)}
