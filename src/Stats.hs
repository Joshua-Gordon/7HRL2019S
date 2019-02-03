module Stats where

data Stats = Stats {
	HP :: Integer,
	AC :: Integer,
	DEF :: Integer,
	STR :: Integer,
	DEX :: Integer,
	INT :: Integer,
	VIS :: Integer,
}

baseStats :: Stats

baseStats = Stats {
	HP = 10,
	AC = 10,
	DEF = 10,
	STR = 10,
	DEX = 10,
	INT = 10,
	VIS = 10,
}

noChange :: Stats -> Stats
noChange = id
