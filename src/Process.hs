{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module Process where
import System.Process
import System.Exit
compileP :: String -> IO String -> IO FilePath -> IO ExitCode
compileP name opt  src = do
    src' <- src
    opt' <- opt
    
    case src' of 
        [] -> do
            putStrLn "No source files found"
            return ExitSuccess
        _  -> system $ "javac -d ./target/" ++ name ++
             " -classpath " ++ opt' ++ ". " ++ ( src') 




