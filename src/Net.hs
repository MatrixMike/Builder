{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Net where
import qualified Data.ByteString.Lazy as ByteString

import Network.HTTP.Client( HttpException( .. ) )
import Network.Wreq
import Control.Lens
import Control.Exception

downLoad :: String -> String -> IO ()
downLoad n u = do
    r <- try $ get u
    case r of
        -- Left (e::HttpException) -> do putStrLn "-------" 
        --                               putStrLn e 
        --                               putStrLn "-------"
        Left (InvalidUrlException s s1) -> do 
                                              putStrLn "-------" 
                                              putStrLn s
                                              putStrLn s1
                                              putStrLn "-------" 
                                              putStrLn ""
        Left (StatusCodeException s _ _) -> do putStrLn $ (show s) ++ " " ++ u
        Left (e) -> do putStrLn $ show e
        Right res -> do 
            case (res ^. responseStatus ^. statusCode) of
                200 -> ByteString.writeFile n bin where bin = res ^. responseBody
                _   -> putStrLn "Bad code " 

 