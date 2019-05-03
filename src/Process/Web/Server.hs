{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Server
  (start)
where

import Web.Scotty
import Data.Default.Class (def)
import Data.String (fromString)
import qualified Network.Wai.Handler.Warp as W


customSettings :: String -> Int -> Options
customSettings ip port = def { verbose = 1
                     , settings = W.setHost (fromString ip) $ settings def
                     }


-- start :: String -> IO ()
-- start ipAddr = scottyOpts $ customSettings ipAddr $ do
--   get "/" $ text "hello world"


start :: String -> Int -> IO ()
start ip port = scottyOpts (customSettings ip port) $ do
  get "/" $ text "hello world"


stop :: IO ()
stop = putStrLn "Stop"
