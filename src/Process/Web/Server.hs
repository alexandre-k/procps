{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Server
  (
    start
  , Server (..)
  )
where

import Data.Default.Class (def)
import Data.String (fromString)
import qualified Network.Wai.Handler.Warp as W
import Process.Web.Internal.Index  (index)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty


data Server = Server
  { ip :: String
  , port :: Int
  } deriving (Show)


customSettings :: String -> Int -> Options
customSettings ip port = def { verbose = 1
                             , settings = W.setHost (fromString ip) $ settings def
                             }


dashboard :: ScottyM ()
dashboard = do
  get "/" $ html (renderHtml index)

start :: Server -> IO ()
start (Server ip port) = scottyOpts (customSettings ip port) $ dashboard
