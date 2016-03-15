
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


--import Control.Monad
--import Data.List
--import System.Directory
--import System.FilePath ((</>))
--import System.IO
--import System.IO.Strict as Strict
mvnURL :: String
mvnURL = "http://central.maven.org/maven2/"


fName :: LibRef -> String
fName l = (artifact l) ++ "-" ++ (version l) ++ ".jar"
-- ----------------------------------------------------------------------------

makeURL :: LibRef -> String
makeURL l = mvnURL  ++ ( dotSlash (grp l)) ++ "/" ++ (artifact l) ++ "/" ++ (version l) ++ "/" ++ (fName l) 
     where dotSlash  = map (\c -> if c == '.' then '/' else c)   
      
-- ----------------------------------------------------------------------------
downLoad' :: LibRef -> IO ()
downLoad' l = do 
  print l
  downLoad (fName l) (makeURL l)
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

--if isModule n proj then compileModule (n proj) else putStrLn $ "Unknown module " ++ (show n)
compile' :: Name -> Project -> IO ()
compile' n proj =  do
  let x = moduleByName n proj
  case x of 
    Left _ -> putStrLn $ "Unknown module " ++ (show n)
    Right m -> do
      putStrLn $ "compiling " ++ (show n)
      compileJava m
      putStrLn "" 
   
-- -----------------------------------------------------------
compileJava :: Module -> IO ExitCode
-- (options m) 
compileJava m = compileP (srcfiles m)

-- java options as in javac <options> <source files>
options :: Module -> String
options = undefined

-- All *.java from srcRoot down in supplied module 
srcfiles :: Module -> IO [FilePath] -- of CSV
srcfiles m =
    case (itemByNameInModule m "sourcepath") of
      Left msg  -> do 
        putStrLn msg
        return [""::FilePath]
      Right (Item ("srcPath", srcFldr) ) -> do
       srcFls <- allJavaFilesFromFolder srcFldr 
       return $  intersperse ", " srcFls

      Right (Item (_, _) ) -> return [""::FilePath]


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
-- ----------------------------------------------------------------------------------------------------
readArgs :: IO ()
readArgs = do
    proj <-  parseProjectFile "project.txt"
    case validateProject proj of
          Left msg -> putStrLn msg
          Right pr -> do
              (command:args) <- getArgs  
              let res = lookup (rmvExtSpaces command) dispatch  
              case res of
                Just action -> action args pr
                Nothing     -> putStrLn ("Error - unknown argument " ++ command)


 -- javac_path : * /bin/...
 --       uber_jar {
 --        jar_name : *
 --        include_module : * mod1, mod2
 