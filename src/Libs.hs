-- module Libs where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
sep :: Char
sep = ':'

ws :: Parser String 
ws = many (oneOf " ")

int :: (Integral a, Read a) => Parser a
int =  fmap read  (many1 digit)

stringLike :: Parser String
stringLike = char '"' *> many (noneOf ['\"', '\r', '\n']) <* char '"'

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

-- eg 'org.apache.felix:org.apache.felix.bundlerepository:2.0.6'
data LibRef = LibRef {grp :: String, artifact :: String, version :: String} deriving (Show)


type Deps = [LibRef]
letterDigUndrDot :: Parser String
letterDigUndrDot = do
   many1 (letter <|> digit <|> char '_' <|> char '.')

libRefParser :: Parser LibRef
libRefParser = do
	grp <- letterDigUndrDot
	char sep
	art <- letterDigUndrDot
	char sep
	ver <- letterDigUndrDot
	return $ LibRef grp art ver




testLibRefParser = do
	let  lref = parse libRefParser "Libref" "org.apache.felix:org.apache.felix.bundlerepository:2.0.6"
	case lref of 
		Left err  ->  show err
		Right val ->  show val


