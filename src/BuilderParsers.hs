{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
module BuilderParsers where

import           BuilderTypes
import           Control.Monad
import           Data.Either
import           Data.List
import           Data.List.Split
import           System.Directory
import           Text.ParserCombinators.Parsec
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


rmvExtSpaces :: String -> String
rmvExtSpaces = unwords . words

itemValue :: Parser String
itemValue =
  many1 (letter <|> digit <|> char '_' <|> char '.' <|> char '-' <|> char '/'  <|> char ',' <|> char ' ' <|> char ':')

letterDigUndrDot :: Parser String
letterDigUndrDot =
   many1 (letter <|> digit <|> char '_' <|> char '.' <|> char '-')

validateProject :: Either String Project -> Either String Project
validateProject p =
 case p of
  Left msg  ->  Left msg
  Right  p' ->  checkModDeps p'  >>=  noDupOptionals >>= noDupModules


-- ----------------------------------------------------------------------------
noDupModules :: Project -> Either String Project
noDupModules p
  | x \\ nub x == [] = Right p
  | otherwise = Left ( "Duplicate module names: " ++ (show diff))
  where
    diff = x \\ nub x
    x = moduleNames p
-- ----------------------------------------------------------------------------
noDupOptionals :: Project -> Either String Project
noDupOptionals p  = do
  let (Build mods) =  buil p
  case (mapM noDupOptionals' mods) of
    Left  m -> Left m
    Right _ -> Right p

-- for each module check for dups in the Items
noDupOptionals' :: Module -> Either String Module
noDupOptionals' m
   | diff == [] = Right m
   | otherwise = Left ( show  (moduleName m) ++  " has duplicate optional names. " ++ (show diff))
   where
    diff = listDups $ itemNames m

listDups :: Eq a => [a] -> [a]
listDups xs = xs \\ nub xs
-- ----------------------------------------------------------------------------
checkModDeps :: Project -> Either String Project
checkModDeps p = do
  let (Build mods) =  buil p
  case (mapM checkModDeps' mods) of
    Left  m -> Left m
    Right _ -> Right p

checkModDeps' :: Module -> Either String Module
checkModDeps' m =
  case itemByName "modDep" $ items m  of -- ls is csv
    Left _              -> Right m
    Right  x -> do
      let (Item (_, ls)) = x
      let lst = splitOn "," ls
      case listDups lst of
        [] -> Right m
        _  -> Left ("Duplicate module names in module dep list: " ++ (show $ moduleName m) ++ " --> " ++ (show ls))
-- -- ----------------------------------------------------------------------------
checkSrcFolder :: Either String Project -> IO (Either String Project)
checkSrcFolder (Left m) = return $ Left m
checkSrcFolder (Right p) = do
  let (Build mods) =  buil p
  errs <- mapM (checkSrcFolder')  mods
  -- any error
  case length (lefts errs) of
    0 -> return $ Right p -- all ok
    _ -> return $ Left (show errs)

checkSrcFolder' :: Module -> IO( Either String Module)
checkSrcFolder' m =
  case itemByNameInModule m "sourcepath" of
    Left _ -> return (Left $ "The module " ++  (moduleName m) ++ " does not have a sourcepath defined.")
    Right (Item (_, srcPath)) -> do
      ex <- doesDirectoryExist srcPath
      if ex then return $ (Right m) else return $ Left ("No such folder " ++ srcPath ++ " for module " ++ (moduleName m))
-- -- ----------------------------------------------------------------------------
data  Args = Args [Name] [Name] deriving (Show)
wd :: Parser String
wd = do
  spaces
  wd <- many1 letter
  spaces
  return wd

argsParser :: Parser Args
argsParser = do
  spaces
  cmds <- many1 wd
  hasM <- optionMaybe (string "-m")
  case hasM of
    Nothing -> return $ Args cmds []
    Just _  -> do
      mods <- many1 wd
      return $ Args cmds mods

--parseArgs :: String -> Either (String Args)
parseArgs args  = 
  case (parse argsParser "Parsing args" args) of
    Left m -> return $ (Left (show m))
    Right a -> return $ Right a



parseProj :: IO (Either String Project)
parseProj =  parseProjectFile "project.txt"

parseProjectFile :: String -> IO (Either String Project)
parseProjectFile fn = do
  prj <- readFile fn
  let p = parse projectParser "Parsing project" prj
  case p of
    Left msg -> do
      print msg
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
  sp
  string envToken
  char openBrace
  sp
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
  group'    <- letterDigUndrDot
  char sep
  artifact' <- letterDigUndrDot
  char sep
  version'  <- letterDigUndrDot
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
  deps'  <-         depsParser
  spaces
  name'  <-         kwip "name"
  spaces
  -- This is not very nice...
  x1     <-  many $ kwip "modDep"
  spaces
  x2     <-  many $ kwip "sourcepath"
  spaces
  x3     <-  many $ kwip "destPath"
  spaces
  x4     <-  many $ kwip "jarName"
  spaces
  x5     <-  many $ kwip "main"
  spaces
  char '}'
  spaces
  let Item (_, mName) = name'
  return $ Module mName ([name'] ++ x1 ++  x2 ++  x3 ++  x4 ++ x5) deps'


-- ----------------------------------------------------------------------------
-- KeyWordItemParser
kwip :: String -> Parser Item
kwip keyWord = do
  spaces
  name <- string keyWord
  spaces
  char ':'
  spaces
  val  <-  itemValue
  spaces
  return $ Item (name, rmvExtSpaces val)
-- ----------------------------------------------------------------------------
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
sp = do 
  spaces
  many simpleComment
  spaces

simpleComment = do
  string "<!--"
  manyTill anyChar (try (string "-->"))





