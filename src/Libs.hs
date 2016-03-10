
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

dispatch :: [(String, [String] -> Project -> IO ())]  
dispatch =  [("clean"  , clean  ),  
             ("compile", compile),
             ("build"  , build  )]

clean args proj =  putStrLn "clean"

build [] proj = putStrLn "build"
build ns proj = putStrLn "build ns"

-- compile | compile <module-name>
compile [] proj = compile (moduleNames proj) proj
compile ns proj = mapM_ (\n -> compile' n proj) ns

compile' n proj = if isModule n proj 
                   then compileModule n
                  else putStrLn "oops"

compileModule m = putStrLn ("compiling module " ++ (show m))
-- -----------------------------------------------------------

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
    proj <- parseProjectFile "project.txt"
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
 