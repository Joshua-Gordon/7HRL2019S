module Slot where

data Side = L | R deriving (Eq,Ord)

data Slot = ControlModule
    | Helmet
    | Armor
    | Torso
    | ArmAugment Side
    | HandAugment Side
    | LegAugment Side
    | Bracer Side
    | Greave Side
    | Boot Side
    deriving (Eq,Ord)
