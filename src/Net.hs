{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Net where
import qualified Data.ByteString.Lazy as ByteString

import Network.HTTP
import Network.URI (parseURI)
import Network.Wreq


downLoad :: String -> String -> IO ()
downLoad n u = do
  f <- get u
  ByteString.writeFile n f
  print n
  print u
 where
    get url = let uri = case parseURI url of
                          Nothing -> error $ "Invalid URI: " ++ url
                          Just u_ -> u_ in
              simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

