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
import Process.Manage (Process(..))
import Process.Monitor (MonitoredProcess(..), listAll, monitoredProcess, stop)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty hiding (status)


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

  post "/api/v1.0/stop/" $ do
    liftIO $ stop $ MonitoredProcess {
      name = "thunderbird"
      , process = Process {pname = "thunderbird"
                        , pid = "11841"
                        , command = "/usr/bin/thunderbird"}
      , started = True
      , stopped = False
      , memoryUsage = 0
      , uptime = 0
      , status = "Running"
      , logFile = "/home/laozi/.procps/logs/thunderbird"}
    text $ pack $ "ok"




start :: Server -> IO ()
start (Server ip port) = scottyOpts (customSettings ip port) $ dashboard
