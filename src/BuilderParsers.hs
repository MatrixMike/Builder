{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
module BuilderParsers where

import Text.ParserCombinators.Parsec
import BuilderTypes


sep :: Char
sep = ':'

openBrace :: Char
openBrace = '{'

closeBrace :: Char
closeBrace = '}'

projectToken :: String
projectToken = "project"

envToken :: String
envToken = "env"


depsToken :: String
depsToken = "deps"


buildToken :: String
buildToken = "build"
deployToken :: String
deployToken = "deploy"

letterDigUndrDot :: Parser String
letterDigUndrDot = do
   many1 (letter <|> digit <|> char '_' <|> char '.' <|> char '-')
-- ----------------------------------------------------------------------------

projectParser :: Parser Project
projectParser = do
  spaces
  string projectToken
  char openBrace
  spaces

  env    <- envParser
  build  <- buildParser
  deploy <- deployParser

  spaces
  char closeBrace

  return $ Project env  build deploy
-- ----------------------------------------------------------------------------
envParser :: Parser Env
envParser = do
  spaces
  string envToken
  char openBrace
  spaces
  char closeBrace
  return $ "env..."
-- ----------------------------------------------------------------------------
depsParser :: Parser Deps
depsParser = do
  spaces
  string depsToken
  char openBrace
  spaces
  manyDep <- many libRefParser
  spaces
  char closeBrace
  return $ manyDep
-- ----------------------------------------------------------------------------
buildParser :: Parser Build
buildParser = do
  spaces
  string buildToken
  char openBrace
  modules <- many1 moduleParser
  char closeBrace
  spaces
  return $ Build modules
-- ----------------------------------------------------------------------------
deployParser :: Parser Deploy
deployParser = do
  spaces
  string deployToken
  char openBrace
  spaces
  char closeBrace
  return $ "deploy..."
-- ----------------------------------------------------------------------------

libRefParser :: Parser LibRef
libRefParser = do
  spaces
  grp <- letterDigUndrDot
  char sep
  art <- letterDigUndrDot
  char sep
  ver <- letterDigUndrDot
  spaces
  return $ LibRef grp art ver
-- ----------------------------------------------------------------------------
-- data Item = (Name, Value)
-- data Module = Module [Item]
itemParser :: Parser Item
itemParser = do
  spaces
  name <- many1 letter
  spaces
  char ':'
  spaces
  val  <- many1 letter
  spaces
  return $ Item (name, val)
-- ----------------------------------------------------------------------------
moduleParser :: Parser Module
moduleParser = do
  spaces
  string "module"
  spaces
  char '{'
  spaces
  deps <-   depsParser
  spaces
  name  <- nameParser
  items <- many1 itemParser
  spaces
  char '}'
  spaces
  return $ Module (name : items) deps
-- ----------------------------------------------------------------------------
nameParser :: Parser Item
nameParser = do
  spaces
  name <- string "name"
  spaces
  char ':'
  spaces
  val  <- many1 letter
  spaces
  return $ Item (name, val)

-- ----------------------------------------------------------------------------

int :: (Integral a, Read a) => Parser a
int =  fmap read  (many1 digit)
-- ----------------------------------------------------------------------------
testLibRefParser = do
  let  lref = parse libRefParser "Libref" "org.apache.felix:org.apache.felix.bundlerepository:2.0.6"
  case lref of 
    Left err  ->  show err
    Right val ->  show val
-- ----------------------------------------------------------------------------
testManyLibRefParser = do
  let  lref = parse (many libRefParser) "Libref" ""
  case lref of 
    Left err  ->  show err
    Right val ->  show val
-- ---------
-------------------------------------------------------------------
testDepsParser text = do 
  let lref = parse depsParser "LibRef" text
  case lref of 
    Left err  ->  show err
    Right val ->  show val
-- ----------------------------------------------------------------------------




