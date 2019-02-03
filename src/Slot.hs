module Slot where

data Side = L | R

data Slot = ControlModule
	| Armor
	| ArmAugment Side
	| HandAugment Side
	| LegAugment Side
	| Bracer Side
	| Greave Side
	| Boot Side
