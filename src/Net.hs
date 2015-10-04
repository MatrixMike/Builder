{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Net where

import qualified Data.ByteString as B
import Network.HTTP
import Network.URI (parseURI)



downLoad :: String -> String -> IO ()
downLoad n u = do
  f <- get u
  B.writeFile n f
  print n
  print u
  print f
 where
    get url = let uri = case parseURI url of
                          Nothing -> error $ "Invalid URI: " ++ url
                          Just u_ -> u_ in
              simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

