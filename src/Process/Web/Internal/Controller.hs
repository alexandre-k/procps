{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Internal.Controller where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Process.Internal.Common as Internal
import Process.Monitor (MonitoredProcess)

listAll :: IO (Maybe [MonitoredProcess])
listAll = do
  confFile <- Internal.configFile
  processes <- B.readFile confFile
  return $ decode processes
