
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

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

runProject :: IO ()
runProject = do
  pr <- readFile "project.txt" 
  let p = parse projectParser "Parsing project" pr
  case p of
    Left msg -> putStrLn $ show  msg
    Right pr -> do 
      print pr

libsMain :: IO ()
libsMain = runProject `catchIOError` errHandler

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("clean",   cleanProj)  
            , ("list" ,   listProj)  
            , ("compile", compileProj)
            , ("build",   buildProj)  
            ]
cleanProj args = do putStrLn "clean"
listProj  []   = do putStrLn "list"
listProj  args = do (putStrLn "list with " )

buildProj   args = do putStrLn "build"
compileProj args = do putStrLn "compile"

readArgs = do
    args <- getArgs
    (command:args) <- getArgs  
    let res = lookup command dispatch  
    case res of
       Just action -> action args
       Nothing     -> do putStrLn ("Error - unknown argument " ++ command)

   

 -- javac_path : * /bin/...
 --       uber_jar {
 --        jar_name : *
 --        include_module : * mod1, mod2
 