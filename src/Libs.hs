-- module Libs where
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import Text.ParserCombinators.Parsec
-- import Control.Applicative hiding ((<|>), optional, many)
sep :: Char
sep = ':'

openBrace :: Char
openBrace = '{'
closeBrace :: Char
closeBrace = '}'

deps :: String
deps = "deps"
env :: String
env = "env"




ws :: Parser String 
ws = many (oneOf " ")

int :: (Integral a, Read a) => Parser a
int =  fmap read  (many1 digit)

-- stringLike :: Parser String
-- stringLike = char '"' *> many (noneOf ['\"', '\r', '\n']) <* char '"'

-- lexeme :: Parser a -> Parser a
-- lexeme p = ws *> p <* ws

-- eg 'org.apache.felix:org.apache.felix.bundlerepository:2.0.6'
data LibRef = LibRef {grp :: String, artifact :: String, version :: String} deriving (Show)


type Deps = [LibRef]

letterDigUndrDot :: Parser String
letterDigUndrDot = do
   many1 (letter <|> digit <|> char '_' <|> char '.')


libRefParser :: Parser LibRef
libRefParser = do
	spaces
	char '"'
	grp <- letterDigUndrDot
	char sep
	art <- letterDigUndrDot
	char sep
	ver <- letterDigUndrDot
	char '"'
	spaces
	return $ LibRef grp art ver

depsParser :: Parser Deps
depsParser = do
	spaces
	string deps
	char openBrace
	spaces
	manyDep <- many libRefParser
	spaces
	char closeBrace
	return $ manyDep

testLibRefParser = do
	let  lref = parse libRefParser "Libref" "org.apache.felix:org.apache.felix.bundlerepository:2.0.6"
	case lref of 
		Left err  ->  show err
		Right val ->  show val


testManyLibRefParser = do
	let  lref = parse (many libRefParser) "Libref" ""
	case lref of 
		Left err  ->  show err
		Right val ->  show val

testDepsParser text = do 
	let lref = parse depsParser "LibRef" text
	case lref of 
		Left err  ->  show err
		Right val ->  show val

	



