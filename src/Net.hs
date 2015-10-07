{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Net where
import qualified Data.ByteString.Lazy as ByteString

import Network.HTTP.Client( HttpException( StatusCodeException ) )
import Network.Wreq
import Control.Lens
import Control.Exception

downLoad :: String -> String -> IO ()
downLoad n u = do
    print u
    r <- try $ get u
    case r of
        -- Left (e::HttpException) -> do print "-------" 
        --                               print e 
        --                               print "-------"
        Left (StatusCodeException s _ _) -> do print s
        Right res -> do 
            case (res ^. responseStatus ^. statusCode) of
                200 -> ByteString.writeFile n bin where bin = res ^. responseBody
                _   -> putStrLn "Bad code " 

 