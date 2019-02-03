module Stats where

data Stats = Stats {
   _HP :: Integer,
   _AC :: Integer,
   _DEF :: Integer,
   _STR :: Integer,
   _DEX :: Integer,
   _INT :: Integer,
   _VIS :: Integer
}

baseStats :: Stats
baseStats = Stats {
   _HP = 10,
   _AC = 10,
   _DEF = 10,
   _STR = 10,
   _DEX = 10,
   _INT = 10,
   _VIS = 10
}

noChange :: Stats -> Stats
noChange = id
