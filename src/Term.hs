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

data Command = Move Dir | Atk (Maybe String) | Ls | Sel String | Alias String String | Nop deriving(Show)

data Term = Term {
  buff :: [String],
  prompt :: String,
  selected :: String,
  aliases :: [(String,String)],
  world :: World
} 

addBuff :: String -> [String] -> [String]
addBuff s b = take 20 (s:b)

tStep :: Float -> Term -> IO Term
tStep f t = getLine >>= flip (process f) t

-- def :: Term
-- def = Term ["Welcome to Vent Crawler 2 (No Relation)"] "you@game:~$" "" []

process :: Float -> String -> Term -> IO Term
process f s t = maybe (print err *> return t{buff=addBuff err (buff t)}) (flip (handleCmd f) t) cmd
  where
    cmd = parse (doAliases (aliases t) s)
    err = "error " ++ s ++ " not a valid command"

handleCmd :: Float -> Command -> Term -> IO Term
handleCmd _ (Alias a b) t = return $ t{aliases = (a,b):(aliases t),buff=addBuff "alias created sucesfully" (buff t)}
handleCmd f Nop t         = updateWorld f (world t) >>= (\w -> return t{world = w}) 
handleCmd _ Ls t          = (sequence $ map (putStrLn . showEnt) (entities . zone . world $ t)) *> putStr "\n" *> return t
handleCmd f (Move d) t    = updateWorld f nw >>= (\w -> return t{world = w})
  where
    w = world t
    p = player w
    e = entity p
    ne = tryMove w d e
    np = p{entity=ne}
    nw = w{player=np}
handleCmd _ _ t           = return t

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

