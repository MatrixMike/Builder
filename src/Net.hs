{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Net where

import qualified Data.ByteString as B
import Network.HTTP
import Network.URI (parseURI)


downLoad :: String -> String -> IO ()
downLoad name u = do
  f <- get u
  B.writeFile name f
 where
    get url = let uri = case parseURI url of
                          Nothing -> error $ "Invalid URI: " ++ url
                          Just u_ -> u_ in
              simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

