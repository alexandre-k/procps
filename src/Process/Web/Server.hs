{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Server
  (
    start
  , Server (..)
  )
where

import Web.Scotty
import Data.Default.Class (def)
import Data.String (fromString)
import qualified Network.Wai.Handler.Warp as W


data Server = Server
  { ip :: String
  , port :: Int
  } deriving (Show)


customSettings :: String -> Int -> Options
customSettings ip port = def { verbose = 1
                             , settings = W.setHost (fromString ip) $ settings def
                             }


-- start :: String -> IO ()
-- start ipAddr = scottyOpts $ customSettings ipAddr $ do
--   get "/" $ text "hello world"

serveStaticFiles :: ScottyM ()
serveStaticFiles = do
  get "/" $ file "./static/html/index.html"

start :: Server -> IO ()
start (Server ip port) = scottyOpts (customSettings ip port) $ serveStaticFiles


stop :: IO ()
stop = putStrLn "Stop"
