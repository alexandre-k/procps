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
import qualified Network.Wai.Handler.Warp as W
import Process.Web.Internal.Index  (index)
import Process.Monitor (MonitoredProcess, listAll)
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

-- processTable :: ActionM ()
-- processTable = do
--   processes <- liftIO listAll
--   case processes of
--     Just p -> do
--       return renderHtml $ index p
--     Nothing -> do
--       return renderHtml $ index []


dashboard :: ScottyM ()
dashboard = do
  get "/" $ do
    processes <- liftIO listAll
    case processes of
      Just p -> html . renderHtml $ index p
      Nothing -> html . renderHtml $ index []


-- registeredProcesses :: [MonitoredProcesses]
-- registeredProcesses = do
--   processes <- listAll
--   case processes of
--     Just j ->


start :: Server -> IO ()
start (Server ip port) = scottyOpts (customSettings ip port) $ dashboard
