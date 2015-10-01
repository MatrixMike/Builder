
module Libs where
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
import Text.ParserCombinators.Parsec
import Net

-- import Control.Applicative hiding ((<|>), optional, many)
sep :: Char
sep = ':'

openBrace :: Char
openBrace = '{'

closeBrace :: Char
closeBrace = '}'

depsToken :: String
depsToken = "deps"

envToken :: String
envToken = "env"


-- eg 'org.apache.felix:org.apache.felix.bundlerepository:2.0.6'
data LibRef = LibRef {grp :: String, artifact :: String, version :: String} deriving (Show)
type Deps = [LibRef]

fName :: LibRef -> String
fName l = (grp l) ++ "-" ++ (version l) ++ ".jar"
-- ----------------------------------------------------------------------------

int :: (Integral a, Read a) => Parser a
int =  fmap read  (many1 digit)
-- ----------------------------------------------------------------------------

letterDigUndrDot :: Parser String
letterDigUndrDot = do
   many1 (letter <|> digit <|> char '_' <|> char '.')

-- ----------------------------------------------------------------------------

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
makeURL l = "http://mvnrepository.com/artifact/" ++ (grp l) ++ "/" ++ (artifact l) ++ "/" ++ (version l) ++ "/" ++ (fName l)


--depRetriever :: Deps -> IO ()
depRetriever deps = do
  mapM_ downLoad (map (\d -> makeURL d) deps)

  
-- http://mvnrepository.com/artifact/io.fabric8/fabric8-arquillian/2.2.38
--http://central.maven.org/maven2/io/fabric8/fabric8-arquillian/2.2.38/fabric8-arquillian-2.2.38.jar

