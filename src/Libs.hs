
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind#-}

module Libs where
import Net
import BuilderParsers
import BuilderTypes
import System.Environment
import System.Process
import Process
import System.Directory
import System.FilePath ((</>))
import System.IO
import Control.Monad
import Data.List
import System.Exit

bldHomeLib :: IO FilePath
bldHomeLib = do
  hm <- getHomeDirectory 
  return $ hm </> ".bldr" </> "lib"

target  :: Name -> IO FilePath 
target mn = do
   dr <-  getCurrentDirectory 
   return $ dr </> "target" </> mn

fqFileName :: String -> IO String 
fqFileName fn = do
  homeLib <- bldHomeLib
  return $ homeLib </> fn

fqFileNameSColon :: String -> IO String
fqFileNameSColon fn = do
  homeLib <- bldHomeLib
  return $ homeLib </> (fn ++ ":")

mvnURL :: String
mvnURL = "http://central.maven.org/maven2/"

fName :: LibRef -> String
fName l = (artifact l) ++ "-" ++ (version l) ++ ".jar"
-- ----------------------------------------------------------------------------

makeURL :: LibRef -> String
makeURL l = mvnURL  ++ ( dotSlash (grp  l)) ++ "/" ++ (artifact l) ++ "/" ++ (version l) ++ "/" ++ (fName l) 
 where dotSlash  = map (\c -> if c == '.' then '/' else c)   
      
-- ----------------------------------------------------------------------------
downLoad' :: LibRef -> IO ()
downLoad' l = do 
  fq     <- fqFileName (fName l)
  exists <- doesFileExist fq
  case exists  of
     True  -> putStrLn ("File previously downloaded. Skipping " ++ (fName l))
     False -> downLoad fq (makeURL l)
-- ----------------------------------------------------------------------------

depRetriever :: [LibRef] -> IO ()
depRetriever  = mapM_ downLoad'  
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
             ("list"   , list   ),
             ("libs"   , ldlibs )]

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
cleanModule m = do
  putStrLn ("cleanimg  module " ++  m)
  t <- target m 
  exists <- doesDirectoryExist t

  case exists of
    True -> do
              removeDirectoryRecursive t
              createDirectoryIfMissing True t 
    False ->  createDirectoryIfMissing True t 
      
    
-- build | build <module-name>
build :: [Name] -> Project -> IO ()
build [] proj = build (moduleNames proj) proj
build ns proj = mapM_ (\n -> build' n proj) ns

build' :: Name -> Project -> IO ()
build' n proj = if isModule n proj then buildModule n else putStrLn $ "Unknown module " ++ (show n)
buildModule m = putStrLn ("building  module " ++ (show m))

-- ldlibs | ldlibs <module-name>
ldlibs :: [Name] -> Project -> IO()
ldlibs [] proj = ldlibs (moduleNames proj) proj
ldlibs mns proj = mapM_ (\n -> ldlibs' n proj) mns

ldlibs' :: Name -> Project -> IO()
ldlibs' n proj = 
  case (moduleByName n proj) of 
    Left _   -> putStrLn $ "Unknown module " ++ (show n)
    Right md -> depRetriever $ deps md

-- compile | compile <module-name>
compile :: [Name] -> Project -> IO ()
compile [] proj = compile (moduleNames proj) proj
compile ns proj = mapM_ (\n -> compile' n proj) ns

--if isModule n proj then compileModule (n proj) else putStrLn $ "Unknown module " ++ (show n)
compile' :: Name -> Project -> IO ()
compile' n proj =  
  case moduleByName n proj of 
    Left _  -> putStrLn $ "Unknown module " ++ (show n)
    Right m -> do
      putStrLn "Compiling..."
      compileJava m
      return () 
   
-- -----------------------------------------------------------
compileJava :: Module -> IO ExitCode
compileJava m = compileP (moduleName m) (options m) (srcfiles m)

-- java options as in javac <options> <source files>
options :: Module -> IO String
options md = do
  let jars = [ (fqFileNameSColon (fName l))  | l <- deps md]
  jars' <- sequence jars
  return (concat  jars')
 
-- All *.java from srcRoot down in supplied module 
srcfiles :: Module -> IO FilePath -- of CSV
srcfiles m =
    case (itemByNameInModule m "sourcepath") of
      Left msg  -> do 
        putStrLn msg
        return ""
      Right (Item ("sourcepath", srcFldr) ) -> do
       srcFls <- allJavaFilesFromFolder srcFldr
       return $   unwords srcFls

      Right (Item (_, _) ) -> return (""::FilePath)


-- ------------------------------------------------------------------------------------------------
--  Some file and folder utilities
-- all files in all folders from root folder where folders and files  satisfy the  filters fltrFld and fltrFile

true :: FilePath -> Bool
true _ = True

isTestFldr :: FilePath -> Bool
isTestFldr = isInfixOf "src\\test\\java"

isJavaFile :: FilePath -> Bool
isJavaFile = isInfixOf ".java"

allJavaFilesFromFolder :: FilePath -> IO [FilePath]
allJavaFilesFromFolder = allFilesFromFolder true isJavaFile 

allFilesFromFolder :: (FilePath -> Bool) -> (FilePath -> Bool) -> FilePath -> IO [FilePath]
allFilesFromFolder fltrFld fltrFile  fp = do
  fldrs <- folders fp
  fls <- mapM filesInFolder (filter fltrFld ( "." : fldrs))
  return $ filter fltrFile (concat fls)


-- all folders/sub folders from a given root
folders :: FilePath -> IO [FilePath]
folders fp  = do
    itms <- listDirectory fp
    z'  <- filterM doesDirectoryExist $ map (fp </>) itms
    x'  <- mapM folders z'
    return $ z' ++ (concat x') 

filesInFolder :: FilePath -> IO [FilePath]
filesInFolder  fp  = do
    itms <- listDirectory fp
    filterM doesFileExist $ map (fp </>) itms

-- -------------------------------------------------------------
readArgs :: IO ()
readArgs = do
    proj' <-  parseProjectFile "project.txt"
    proj  <-  checkSrcFolder proj'

    case validateProject proj of
          Left msg ->  
            putStrLn msg
          Right pr -> do
              putStrLn "project file parses ok."
              (command:args) <- getArgs  
              let res = lookup (rmvExtSpaces command) dispatch  
              case res of
                Just action -> action args pr
                Nothing     -> putStrLn ("Error - unknown argument " ++ command)

 