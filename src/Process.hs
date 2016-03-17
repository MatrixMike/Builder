{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module Process where
import System.Process
import System.Exit
compileP :: IO [String] -> IO[FilePath] -> IO ExitCode
compileP opt  src = do
	src' <- src
	opt' <- opt
	let optStr = show opt' 

	putStrLn $ "-cp " ++ optStr
	case src' of 
		[] -> do
			putStrLn "No source files found"
			return ExitSuccess
		_  -> (system $ "javac -cp " ++ optStr ++ " " ++ (show srcFiles)) where (srcFiles:_) = src'
