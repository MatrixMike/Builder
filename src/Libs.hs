
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind#-}

module Libs where
import Net
import BuilderParsers
import BuilderTypes
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

depRetriever :: [LibRef] -> IO ()
depRetriever depends = 
    mapM_ downLoad'  depends
-- ----------------------------------------------------------------------------

errHandler :: IOError -> IO ()  
errHandler e = putStrLn $ "ERROR! " ++ (show e) 
   -- | InvalidUrlException e = putStrLn "The file doesn't exist!"    
    -- | otherwise = ioError e  
-- ----------------------------------------------------------------------------
     
-- libsMain :: IO ()
-- libsMain = (parseProjectFile "project.txt" ) `catchIOError` errHandler

dispatch :: [(String, [String] -> Project -> IO ())]  
dispatch =  [("clean"  , clean  ),  
             ("compile", compile),
             ("build"  , build  ),
             ("list"   , list   )]

-- list | list <module-name>
list :: [Name] -> Project -> IO ()
list [] proj = list (moduleNames proj) proj
list ns proj = mapM_ (\n -> list' n proj) ns

list' :: Name -> Project -> IO ()
list' n proj = if isModule n proj then listModule n else putStrLn $ "Unknown module " ++ (show n)

listModule m = putStrLn ("listing  module " ++ (show m))

-- clean | clean <module-name>
clean :: [Name] -> Project -> IO ()
clean [] proj = clean (moduleNames proj) proj
clean ns proj = mapM_ (\n -> clean' n proj) ns

clean' :: Name -> Project -> IO ()
clean' n proj = if isModule n proj then cleanModule n else putStrLn $ "Unknown module " ++ (show n)
cleanModule m = putStrLn ("cleanimg  module " ++ (show m))

-- build | build <module-name>
build :: [Name] -> Project -> IO ()
build [] proj = build (moduleNames proj) proj
build ns proj = mapM_ (\n -> build' n proj) ns

build' :: Name -> Project -> IO ()
build' n proj = if isModule n proj then buildModule n else putStrLn $ "Unknown module " ++ (show n)
buildModule m = putStrLn ("building  module " ++ (show m))

-- compile | compile <module-name>
compile :: [Name] -> Project -> IO ()
compile [] proj = compile (moduleNames proj) proj
compile ns proj = mapM_ (\n -> compile' n proj) ns

compile' :: Name -> Project -> IO ()
compile' n proj = if isModule n proj then compileModule n  else putStrLn $ "Unknown module " ++ (show n)
compileModule m = putStrLn ("compiling module " ++ (show m))
-- -----------------------------------------------------------


rmvExtSpaces :: String -> String
rmvExtSpaces = unwords . words

readArgs :: IO ()
readArgs = do
    proj <-  parseProjectFile "project.txt"
    case validateProject (proj) of
          Left msg -> putStrLn msg
          Right pr -> do
              (command:args) <- getArgs  
              let res = lookup (rmvExtSpaces command) dispatch  
              case res of
                Just action -> action args pr
                Nothing     -> do putStrLn ("Error - unknown argument " ++ command)

 -- ghc -o blldr Main.hs   

 -- javac_path : * /bin/...
 --       uber_jar {
 --        jar_name : *
 --        include_module : * mod1, mod2
 