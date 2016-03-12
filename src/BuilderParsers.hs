{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
module BuilderParsers where

import Text.ParserCombinators.Parsec
import BuilderTypes
import Data.List

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

validateProject :: Either String Project -> Either String Project
validateProject p' = 
 case p' of 
  Left msg ->  Left msg
  Right  p ->  checker p >>= checker' >>=  checker'' >>= noDupModules

checker :: Project -> Either String Project 
checker p = Right p 

checker' :: Project -> Either String Project 
checker' p = Right p 

checker'' :: Project -> Either String Project 
checker'' p = Right p 

-- ----------------------------------------------------------------------------
noDupModules :: Project -> Either String Project 
noDupModules p 
  | x \\ nub x == [] = Right p
  | otherwise = Left ( "Duplicate module names: " ++ (show diff))
  where
    diff = x \\ nub x 
    x = moduleNames p   
-- ----------------------------------------------------------------------------
-- noDupOptionals :: Project -> Either String Project
-- noDupOptionals p  = do 
--   let mods =  buil p
--   -- check each m in turn 
--   map (\x -> noDupOptionals')  mods



-- for each module check for dups in the Items
noDupOptionals' :: Module -> Either String Module
noDupOptionals' m
   | x \\ nub x == [] = Right m
   | otherwise = Left ( show  m ++  " has duplicate optional  names  " ++ (show diff))
   where
    diff = x \\ nub x
    x = itemNames m


parseProj :: IO (Either String Project)
parseProj =  parseProjectFile "project.txt"

parseProjectFile :: String -> IO (Either String Project)
parseProjectFile fn = do
  prj <- readFile fn
  let p = parse projectParser "Parsing project" prj
  case p of
    Left msg -> do 
      putStrLn (show msg)
      return (Left $ show msg)
    Right pr -> return $ Right pr
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




