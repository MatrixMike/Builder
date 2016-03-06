
module Libs where
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import Text.ParserCombinators.Parsec
import Data.Char
import Net
import System.IO
import System.IO.Error
import Network.Wreq
import Network.HTTP.Client

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

mvnURL :: String
mvnURL = "http://central.maven.org/maven2/"




type Env = String
type Deps = [LibRef]
-- eg 'org.apache.felix:org.apache.felix.bundlerepository:2.0.6'
data LibRef = LibRef {grp :: String, artifact :: String, version :: String} deriving (Show)


data Build = Build [Module] deriving (Show)
type Name = String
type Value = String
data Item = Item (Name, Value) deriving (Show)
data Module = Module {items :: [Item], deps :: Deps}  deriving (Show)


type Deploy = String

data Project = Project {env :: Env, build :: Build, deploy :: Deploy} deriving (Show)

fName :: LibRef -> String
fName l = (artifact l) ++ "-" ++ (version l) ++ ".jar"
-- ----------------------------------------------------------------------------

int :: (Integral a, Read a) => Parser a
int =  fmap read  (many1 digit)
-- ----------------------------------------------------------------------------
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







main = putStrLn "Hello World"
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
-- ----------------------------------------------------------------------------
testDepsParser text = do 
  let lref = parse depsParser "LibRef" text
  case lref of 
    Left err  ->  show err
    Right val ->  show val
-- ----------------------------------------------------------------------------

-- data LibRef = LibRef {grp :: String, artifact :: String, version :: String} deriving (Show)
makeURL :: LibRef -> String
makeURL l = mvnURL  ++ ( dotSlash (grp l)) ++ "/" ++ (artifact l) ++ "/" ++ (version l) ++ "/" ++ (fName l) 
     where dotSlash s = map (\c -> if c == '.' then '/' else c) s  
      
-- ----------------------------------------------------------------------------
downLoad' :: LibRef -> IO ()
downLoad' l = do 
  putStrLn $ show l
  downLoad (fName l) (makeURL l)
-- ----------------------------------------------------------------------------

-- depRetriever :: [LibRef] 
depRetriever deps = 
    mapM_ downLoad'  deps
-- ----------------------------------------------------------------------------

errHandler :: IOError -> IO ()  
errHandler e = putStrLn $ "ERROR! " ++ (show e) 
   -- | InvalidUrlException e = putStrLn "The file doesn't exist!"    
    -- | otherwise = ioError e  
-- ----------------------------------------------------------------------------



runProject :: IO ()
runProject = do
-- main = do
  -- d <- readFile "deps.txt" 
  -- let parsedDeps = parse depsParser "Parsing deps" d

  -- putStrLn $ show parsedDeps
  -- case parsedDeps of 
  --   Left msg -> putStrLn $ show  msg
  --   Right v  -> depRetriever v

-- main = do
  pr <- readFile "project.txt" 
  let p = parse projectParser "Parsing project" pr
  case p of
    Left msg -> putStrLn $ show  msg
    Right pr -> do 
      print pr


      -- depRetriever $ deps pr

libsMain :: IO ()
libsMain = runProject `catchIOError` errHandler


 --    clean , clean <module> , compile, compile <module> , build, build <module.
 -- javac_path : * /bin/...
 --       uber_jar {
 --        jar_name : *
 --        include_module : * mod1, mod2
