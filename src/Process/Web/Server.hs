{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Server
  (
    start
  , Server (..)
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.String (fromString)
import Data.Text.Lazy (pack)
import qualified Network.Wai.Handler.Warp as W
import Process.Web.Internal.Index  (index)
import Process.Monitor (listAll, monitoredProcess)
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
  get "/" $ do
    processes <- liftIO listAll
    case processes of
      Just p -> html . renderHtml $ index p
      Nothing -> html . renderHtml $ index []

  post "/api/v1.0/start/" $ do
    process <- liftIO $ monitoredProcess "thunderbird" "/usr/bin/thunderbird"
    text $ pack $ show process


start :: Server -> IO ()
start (Server ip port) = scottyOpts (customSettings ip port) $ dashboard
