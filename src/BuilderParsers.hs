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

itemValue :: Parser String
itemValue = do
   many1 (letter <|> digit <|> char '_' <|> char '.' <|> char '-' <|> char '/' )
   
letterDigUndrDot :: Parser String
letterDigUndrDot = do
   many1 (letter <|> digit <|> char '_' <|> char '.' <|> char '-')

-- ----------------------------------------------------------------------------
parseProj :: IO (Maybe Project)
parseProj =  parseProjectFile "project.txt"
parseProjectFile :: String -> IO (Maybe Project)
parseProjectFile fn = do
  prj <- readFile fn
  let p = parse projectParser "Parsing project" prj
  case p of
    Left msg -> do 
      putStrLn (show msg)
      return Nothing
    Right pr -> return $ Just pr

-- ----------------------------------------------------------------------------

projectParser :: Parser Project
projectParser = do
  spaces >> string projectToken >> char openBrace >> spaces

  envSection    <- envParser
  buildSection  <- buildParser
  deploySection <- deployParser

  spaces
  char closeBrace

  return $ Project envSection  buildSection deploySection
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
  group' <- letterDigUndrDot
  char sep
  artifact' <- letterDigUndrDot
  char sep
  version' <- letterDigUndrDot
  spaces
  return $ LibRef group' artifact' version'
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
  val  <- itemValue
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
  deps' <-   depsParser
  spaces
  name'  <- kwip "name"
  spaces
  -- This is not very nice...
  x1 <-  many $ kwip "srcPath" 
  spaces
  x2 <-  many $ kwip "classPath"
  spaces
  x3 <-  many $ kwip "destPath"
  spaces
  x4 <-  many $ kwip "jarName"
  spaces
  x5 <-  many $ kwip "main"
  spaces
  char '}'
  spaces
  return $ Module ([name'] ++ x1 ++  x2 ++  x3 ++  x4 ++  x5) deps'

      -- srcPath   : bb/dd/dd

      --    classPath : b/dd/dd
      --    destPath  : b/dd/dd
      --    jarName   : ee
      --    main      : bb/dd/dd/
kwiParsers :: [Parser Item]  
kwiParsers = [kwip "srcPath", kwip "classPath", kwip "destPath", kwip "jarName", kwip "main"] 

   
-- ----------------------------------------------------------------------------
kwip :: String -> Parser Item
kwip keyWord = do
  spaces
  name <- string keyWord
  spaces
  char ':'
  spaces
  val  <-  itemValue
  spaces
  return $ Item (name, val)



int :: (Integral a, Read a) => Parser a
int =  fmap read  (many1 digit)
-- ----------------------------------------------------------------------------
testLibRefParser :: String
testLibRefParser = do
  let  lref = parse libRefParser "Libref" "org.apache.felix:org.apache.felix.bundlerepository:2.0.6"
  case lref of 
    Left err  ->  show err
    Right val ->  show val
-- ----------------------------------------------------------------------------
testManyLibRefParser :: String
testManyLibRefParser = do
  let  lref = parse (many libRefParser) "Libref" ""
  case lref of 
    Left err  ->  show err
    Right val ->  show val
-------------------------------------------------------------------
testDepsParser :: String -> String
testDepsParser text = do 
  let lref = parse depsParser "LibRef" text
  case lref of 
    Left err  ->  show err
    Right val ->  show val
-- ----------------------------------------------------------------------------




