module Consts where

import Graphics.Gloss

_WIDTH = 720
_HEIGHT = 480
_TILESIZE :: Float
_TILESIZE = fromIntegral 24

sq :: Picture
sq = Polygon [(-_TILESIZE/2,-_TILESIZE/2),(_TILESIZE/2,-_TILESIZE/2),(_TILESIZE/2,_TILESIZE/2),(-_TILESIZE/2,_TILESIZE/2)]


walkCycleTime :: Float
walkCycleTime = 0.5

movingWalkCycleTime :: Float
movingWalkCycleTime = 0.5 * walkCycleTime

moveInterpTime :: Float
moveInterpTime = 0.25

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
