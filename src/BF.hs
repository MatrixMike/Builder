{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import Text.Parsec
import Text.Parsec.String
import Control.Monad.State
import qualified Data.IntMap as M
import Data.Word
import Data.Maybe
import System.Environment
import System.Console.GetOpt
import System.Exit
data BFInstruction = GoBack | GoForward | Increment | Decrement | Input | Output 
 | Loop [BFInstruction] deriving (Show)

parseComment :: Parser()
parseComment = do
    many (noneOf "<>,+-.[]")
    return ()

parseInstruction :: Parser BFInstruction
parseInstruction = do
    parseComment
    inst <- parseBack <|> parseForward <|> parseIncrement <|> parseDecrement
                      <|> parseInput   <|> parseOutput    <|> parseLoop
    parseComment
    return inst

parseInstructions :: Parser [BFInstruction]
parseInstructions  = many parseInstruction 
    


parseGen  :: Char -> BFInstruction -> Parser BFInstruction
parseGen c i = char c >> return i

parseBack :: Parser BFInstruction
parseBack = parseGen '<' GoBack

parseForward :: Parser BFInstruction
parseForward = parseGen '>' GoForward 

parseIncrement :: Parser BFInstruction
parseIncrement = parseGen '+' Increment

parseDecrement :: Parser BFInstruction
parseDecrement = parseGen '-' Decrement

parseInput :: Parser BFInstruction
parseInput = parseGen ',' Input

parseOutput :: Parser BFInstruction
parseOutput = parseGen '.' Output

parseLoop :: Parser BFInstruction
parseLoop = do
    char '['
    ins <- parseInstructions
    char ']'
    return $ Loop ins

type BFRunner  = StateT (Int, M.IntMap Word8) IO ()

-- The maybe function takes a default value, a function, and a Maybe value.
-- If the Maybe value is Nothing, the function returns the default value.
-- Otherwise, it applies the function to the value inside the Just and returns the result.
zeroise :: Maybe Word8 -> Word8
zeroise = maybe 0 id

runInstruction :: BFInstruction -> BFRunner
runInstruction GoBack    = modify (\(h, m) -> (h - 1, m))
runInstruction GoForward = modify (\(h, m) -> (h + 1, m))
runInstruction Increment = do
    (bfHd, bfMap) <- get
    let val = zeroise (M.lookup bfHd bfMap)
    put (bfHd, M.insert bfHd (val + 1) bfMap)
runInstruction Decrement = do
    (bfHd, bfMap) <- get
    let val = zeroise (M.lookup bfHd bfMap)
    put (bfHd, M.insert bfHd (val - 1) bfMap)
runInstruction Input = do
    (bfHd, bfMap) <- get
    c <- liftIO getChar
    put (bfHd, M.insert bfHd (fromIntegral (fromEnum c)) bfMap )
runInstruction Output = do
    (bfHd, bfMap) <- get
    let val = zeroise (M.lookup bfHd bfMap)
    liftIO $ putChar $ toEnum $ fromIntegral val
runInstruction loop@(Loop instns) = do
    (bfHd, bfMap) <- get
    let val = zeroise (M.lookup bfHd bfMap)
    case val of 
        0 -> return ()
        _ -> runInstructions instns >> runInstruction loop

runInstructions :: [BFInstruction] -> BFRunner
runInstructions = mapM_ runInstruction
-- -----------------------
data Action = ParseOnly | Interpret deriving (Show, Eq)
data Options = Options
               {   optHelp    :: Bool
                 , optVersion :: Bool
                 , optAction  :: Action} deriving (Show)

defaultOptions :: Options
defaultOptions = Options
               {   optHelp    = False
                 , optVersion = False
                 , optAction  = Interpret} 

options :: [OptDescr (Options -> Options)]                 
options =
    -- Option [Char] [String] (ArgDescr a) String    

    [ Option ['v'] ["version"] 
        (NoArg $ (\opts -> opts {optVersion = True}))       "Show the version.",
      Option ['h'] ["help"   ] 
        (NoArg $ (\opts -> opts {optHelp    = True}))       "Help.",
      Option ['p'] ["parse"  ] 
        (NoArg $ (\opts -> opts {optAction  = ParseOnly}))  "Parse Only.",   
      Option ['i'] ["Interpret"  ] 
        (NoArg $ (\opts -> opts {optAction  = Interpret}))  "Interpret."    
    ]

usage :: [Char]
usage = "Usage : bf [OPTION...] fileName.\n\n"

bfOptions :: [String] -> IO (Options, Maybe String)
bfOptions argv = 
    case getOpt Permute options argv of
        (o, [n], []  ) -> return (foldl (flip id) defaultOptions o, Just n)
        (o,  _,  []  ) -> return (foldl (flip id) defaultOptions o, Nothing)
        (_,  _,  errs) -> ioError $ userError $ concat errs ++ usage
    


main :: IO ()
main = do

    argv <- getArgs
    (opts, fname) <- bfOptions argv
    print opts
    when (optVersion opts) $ do
        putStrLn "Version 1"
        exitSuccess
    when (optHelp opts) $ do
        putStrLn usage
        exitSuccess
    when (fname == Nothing ) $ do
        putStrLn "Specify one and only one file."
        putStrLn usage
        exitFailure

    let fname' = fromJust fname
    let action = optAction opts
    content <- readFile fname'

    case parse parseInstructions fname' content of
        Left e     -> print e
        Right insns -> do 
            if action == ParseOnly
                then print insns
                else evalStateT (runInstructions insns) (0, M.empty)




