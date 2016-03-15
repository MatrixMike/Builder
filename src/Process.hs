{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module Process where
import System.Process
import System.Exit
compileP :: String -> IO([FilePath]) -> IO (ExitCode)
compileP opts src = do
    (src':_) <- src
    (system $ "javac " ++ opts ++ " " ++ src')

main = do
    system "java -version"
