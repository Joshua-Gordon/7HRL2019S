module Term where

import Text.ParserCombinators.ReadP
import World
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
  aliases :: [(String,String)]
  world :: World
} deriving (Show)

addBuff :: String -> [String] -> [String]
addBuff s b = take 20 (s:b)

step :: Float -> Term -> IO Term
step f t = (putStr (prompt t)) *> getLine >>= flip (process f) t

def :: Term
def = Term ["Welcome to Vent Crawler 2 (No Relation)"] "you@game:~$" "" []

process :: Float -> String -> Term -> IO Term
process f s t = maybe (return t{buff=addBuff ("error " ++ s ++ "not a valid command") (buff t)}) (flip (handleCmd f) t) cmd
  where
    cmd = parse (doAliases (aliases t) s)

handleCmd :: Float -> Command -> Term -> IO Term
handleCmd _ (Alias a b) t = t{aliases = (a,b):(aliases t),buff=addBuff "alias created sucesfully" (buff t)}
handleCmd f Nop t = updateWorld f (world t) >>= (\w -> t{world = w}) 
handleCmd _ t = t

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
parseAlias = string "alias " *> liftA2 Alias (munch (/= ' ')) (string " " *> munch (/= ' '))

