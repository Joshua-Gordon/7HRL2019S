module Consts where

walkCycleTime :: Float
walkCycleTime = 0.5

movingWalkCycleTime :: Float
movingWalkCycleTime = 0.5 * walkCycleTime

moveInterpTime :: Float
moveInterpTime = 0.1

damageAnimTime :: Float
damageAnimTime = 0.25

damageAnimIntensity :: Float
damageAnimIntensity = 45

toHitDie :: (Integer, Integer)
toHitDie = (1, 20)

improvPenalty :: Integer
improvPenalty = 5

improvDamageDie :: Integer
improvDamageDie = 4

improvDamageBonus :: Integer
improvDamageBonus = 0
