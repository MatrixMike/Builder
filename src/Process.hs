{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module Process where
import System.Process

compileP :: String -> [FilePath] -> IO ()
compileP opts src = undefined

main = do
	system "java -version"
