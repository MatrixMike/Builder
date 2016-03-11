
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind#-}

module Libs where
import Data.Char
import Net
import System.IO
import System.IO.Error
import Network.Wreq
import Network.HTTP.Client
import Text.ParserCombinators.Parsec
import BuilderParsers
import BuilderTypes
import System.Console.GetOpt
import System.Environment

mvnURL :: String
mvnURL = "http://central.maven.org/maven2/"


fName :: LibRef -> String
fName l = (artifact l) ++ "-" ++ (version l) ++ ".jar"
-- ----------------------------------------------------------------------------

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

parseProjectFile :: String -> IO (Maybe Project)
parseProjectFile fn = do
  pr <- readFile fn
  let p = parse projectParser "Parsing project" pr
  case p of
    Left msg -> do 
      putStrLn (show msg)
      return Nothing
    Right pr -> return $ Just pr
      
-- libsMain :: IO ()
-- libsMain = (parseProjectFile "project.txt" ) `catchIOError` errHandler

dispatch :: [(String, [String] -> Project -> IO ())]  
dispatch =  [("clean"  , clean  ),  
             ("compile", compile),
             ("build"  , build  ),
             ("list"   , list   )]

-- list | list <module-name>
list [] proj = list (moduleNames proj) proj
list ns proj = mapM_ (\n -> list' n proj) ns
list' n proj = if isModule n proj then listModule n else putStrLn $ "Unknown module " ++ (show n)
listModule m = putStrLn ("listing  module " ++ (show m))

-- clean | clean <module-name>
clean [] proj = clean (moduleNames proj) proj
clean ns proj = mapM_ (\n -> clean' n proj) ns
clean' n proj = if isModule n proj then cleanModule n else putStrLn $ "Unknown module " ++ (show n)
cleanModule m = putStrLn ("cleanimg  module " ++ (show m))

-- build | build <module-name>
build [] proj = build (moduleNames proj) proj
build ns proj = mapM_ (\n -> build' n proj) ns
build' n proj = if isModule n proj then buildModule n else putStrLn $ "Unknown module " ++ (show n)
buildModule m = putStrLn ("building  module " ++ (show m))

-- compile | compile <module-name>
compile [] proj = compile (moduleNames proj) proj
compile ns proj = mapM_ (\n -> compile' n proj) ns
compile' n proj = if isModule n proj then compileModule n  else putStrLn $ "Unknown module " ++ (show n)
compileModule m = putStrLn ("compiling module " ++ (show m))
-- -----------------------------------------------------------

parseProj =  parseProjectFile "project.txt"

rmvExtSpaces :: String -> String
rmvExtSpaces = unwords . words

readArgs = do
    proj <- parseProjectFile "project.txt"
    args <- getArgs   
    case proj of 
      Nothing ->  putStrLn "error"
      Just p  ->  do
        (command:args) <- getArgs  
        let res = lookup (rmvExtSpaces command) dispatch  
        case res of
           Just action -> action args p
           Nothing     -> do putStrLn ("Error - unknown argument " ++ command)

 -- ghc -o blldr Main.hs   

 -- javac_path : * /bin/...
 --       uber_jar {
 --        jar_name : *
 --        include_module : * mod1, mod2
 