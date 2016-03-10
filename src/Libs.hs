
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

parseProjectFile :: String -> IO (Maybe Project)
parseProjectFile fn = do
  pr <- readFile fn
  let p = parse projectParser "Parsing project" pr
  case p of
    Left msg -> do 
      putStrLn (show msg)
      return Nothing
    Right pr -> return $ Just pr
      

-- parseProjectFile' :: [String] -> IO ()

-- libsMain :: IO ()
-- libsMain = (parseProjectFile "project.txt" ) `catchIOError` errHandler

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("clean",   clean)  
            , ("compile", compile)
            , ("build",   build)  
           -- , ("parse",   parseProj)
            ]
clean args   =  putStrLn "clean"


build  [] = putStrLn "build"
build (n:ns) =  putStrLn "build ns"

-- compile | compile <module-name>
compile [] = do
  proj <- parseProj
  case proj of
    Just p -> compile (moduleNames p)
    Nothing -> print "Please correct." 

compile ns = mapM_ compile' ns

compile' n = do
  proj <- parseProjectFile "project.txt"
  case proj of
    Nothing ->  putStrLn "error"
    Just p  ->  
        if isModule n p then compileModule n
          else putStrLn "oops"

compileModule m = 
  putStrLn ("compiling module " ++ (show m))
parseProj        =  parseProjectFile "project.txt"

rmvExtSpaces :: String -> String
rmvExtSpaces = unwords . words 

x :: IO ()
x  = do
  proj <- parseProj
  case proj of
    Just p -> print (moduleNames p)
    Nothing -> print "Please correct." 


readArgs = do
    args <- getArgs
    (command:args) <- getArgs  
    let res = lookup (rmvExtSpaces command) dispatch  
    case res of
       Just action -> action args
       Nothing     -> do putStrLn ("Error - unknown argument " ++ command)

 -- ghc -o blldr Main.hs   

 -- javac_path : * /bin/...
 --       uber_jar {
 --        jar_name : *
 --        include_module : * mod1, mod2
 