
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


mvnURL :: String
mvnURL = "http://central.maven.org/maven2/"


fName :: LibRef -> String
fName l = (artifact l) ++ "-" ++ (version l) ++ ".jar"
-- ----------------------------------------------------------------------------


main = putStrLn "Hello World"
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
