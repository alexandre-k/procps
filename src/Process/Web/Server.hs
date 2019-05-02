{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Server where

import Web.Scotty
import Data.Default.Class (def)
import Data.Monoid (mconcat)
import qualified Network.Wai.Handler.Warp as W


customSettings :: Options
customSettings = def { verbose = 1
                     , settings = W.setHost "127.0.0.1" $ settings def
                     }


-- start :: String -> IO ()
-- start ipAddr = scottyOpts $ customSettings ipAddr $ do
--   get "/" $ text "hello world"


start :: IO ()
start = scottyOpts customSettings $ do
  get "/" $ text "hello world"


stop :: IO ()
stop = putStrLn "Stop"
