{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module Process where
import System.Process
import System.Exit
compileP :: IO([FilePath]) -> IO (ExitCode)
compileP  src = do
    (src':_) <- src
    (system $ "javac " ++ src')

main = do
    system "java -version"
