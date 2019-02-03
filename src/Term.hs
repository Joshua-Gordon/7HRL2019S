module Term where

import Text.ParserCombinators.ReadP
import Graphics.Gloss
import World
import Player
import Entity
import Zone
import Dir
import Data.Maybe
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative
import Graphics.Gloss.Interface.IO.Game

data Command = Move Dir | Atk (Maybe String) | Ls | Sel String | Alias String String | Nop deriving(Show)

data Term = Term {
  buff :: [String],
  prompt :: String,
  selected :: String,
  aliases :: [(String,String)],
  world :: World
} 

clearBuff :: Term -> Term
clearBuff t = t{buff=(addBuff "" (buff t))}

thicAddBuff :: [String] -> [String] -> [String]
thicAddBuff xs ys = take 5 (xs ++ ys)

addBuff :: String -> [String] -> [String]
addBuff s b = take 5 (s:b)

tStep :: Float -> Term -> IO Term
tStep = const return

-- def :: Term
-- def = Term ["Welcome to Vent Crawler 2 (No Relation)"] "you@game:~$" "" []

process :: String -> Term -> IO Term
process s t = maybe (return t{buff=addBuff "" . addBuff err $ (buff t)}) (flip handleCmd t) cmd
  where
    cmd = parse (doAliases (aliases t) s)
    err = "error " ++ s ++ " not a valid command"

handleCmd :: Command -> Term -> IO Term
handleCmd (Alias a b) t = return $ t{aliases = (a,b):(aliases t),buff=addBuff "alias created sucesfully" (buff t)}
handleCmd Nop t         = updateWorld 0 (world t) >>= (\w -> return t{world = w}) 
handleCmd Ls t          = return t{buff=(thicAddBuff (map showEnt (entities . zone . world $ t)) (buff t) )}
handleCmd (Move d) t    = updateWorld 0 nw >>= (\w -> return t{buff=addBuff " " (buff t),world = w})
  where
    w = world t
    p = player w
    e = entity p
    ne = tryMove w d e
    np = p{entity=ne}
    nw = w{player=np}
handleCmd _ t           = return t

showEnt :: Entity -> String
showEnt e = show (position e)

doAliases :: [(String,String)] -> String -> String
doAliases [] = id
doAliases (x:xs) = appAlias x . (doAliases xs)

appAlias :: (Eq a) => ([a],[a]) -> [a] -> [a]
appAlias _ [] = []
appAlias (a,b) xs = if a `isPrefixOf` xs then b ++ (appAlias (a,b) (drop (length a) xs)) else (head xs) : (appAlias (a,b) (tail xs))

parse :: String -> Maybe Command
parse = (fmap fst) . listToMaybe . filter (null . snd) . readP_to_S parser

parser :: ReadP Command
parser = parseMove <|> parseLs <|> parseAtk <|> parseSel <|> parseNop <|> parseAlias

parseMove = string "mv " *> fmap Move parseDir
parseDir = (string  "up" *> pure U) <|> (string  "down" *> pure D) <|> (string  "right" *> pure R) <|> (string  "left" *> pure L) 
parseNop = string "wait" *> pure Nop
parseAtk = (string "atk") *> fmap Atk ((string "" *> pure Nothing) <|>  string " " *> fmap Just (munch (const True)))
parseLs = string "ls"  *> pure Ls
parseSel = string "sel " *> (fmap Sel (munch (const True)))
parseAlias = string "alias " *> liftA2 Alias (munch (/= '=')) (string "=" *> munch (const True))

handleInput :: Event -> Term -> IO Term
handleInput e term = case e of
                            (EventKey (SpecialKey KeyEnter) Up _ _) -> process (head $ buff term) term
                            (EventKey (SpecialKey KeyBackspace) Up _ _) -> return term{buff=(init . head . buff $ term) : tail (buff term)}
                            (EventKey (Char c) Up _ _) -> if c == '\b' then doBackspace term else return term{buff=((head $ buff term)++[c]) : tail (buff term)}
                            _ -> return term

doBackspace :: Term -> IO Term
doBackspace term = if null . head . buff $ term then return term else return term{buff=(init . head . buff $ term) : tail (buff term)}
