module GameData where

import Graphics.Gloss

import Entity
import Item
import Stats
import Slot 


goComp :: Item
goComp = Item {
    name = "Go Compiler; an item held by rats for testing purposes",
    applyStats = \s -> s{_HP=_HP s + 1},
    slotting = [Boot L],
    weaponClass = NotAWeapon,
    damageDie = 4,
    damageBonus = 1
}

getRat :: (Integer,Integer) -> IO Entity
getRat pos = do
               r <- loadBMP "../res/ratR.bmp"
               l <- loadBMP "../res/ratL.bmp"
               let e = getEmptyEntity pos (SimpleCycle r l)
               return $ pickup e Stack {item=goComp,amount=1}
               
