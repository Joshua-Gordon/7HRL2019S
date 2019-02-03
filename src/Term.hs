module Term where

import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative

data World = World

data Command = Move Dir | Atk (Maybe String) | Ls | Sel String | Nop deriving(Show)

data Dir = U | D | L | R deriving(Show)

data Term = Term {
  buff :: String,
  prompt :: String
}

process :: String -> World -> World
process = undefined

parse :: String -> Maybe Command
parse = (fmap fst) . listToMaybe . filter (null . snd) . readP_to_S parser

parser :: ReadP Command
parser = parseMove <|> parseLs <|> parseAtk <|> parseSel <|> parseNop


parseMove = string "mv " *> fmap Move parseDir
parseDir = (string  "up" *> pure U) <|> (string  "down" *> pure D) <|> (string  "right" *> pure R) <|> (string  "left" *> pure L) 
parseNop = string "wait" *> pure Nop
parseAtk = (string "atk") *> fmap Atk ((string "" *> pure Nothing) <|>  (fmap Just look))
parseLs = string "ls"  *> pure Ls
parseSel = string "sel " *> (fmap Sel look)


{--
 ls
 maintain selected enemy
 atk to attack selected enemey
--}
