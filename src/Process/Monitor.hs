{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Process.Monitor
  ( MonitoredProcess (..)
  , listAll
  , monitoredProcess
  , startM
  )
where

import qualified Process.Internal.Common as Internal
import Process.Manage (Process(..), isAlive, kill, start)
import Control.Monad as M
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import GHC.Generics
import System.Exit
import System.FilePath.Posix
import System.Process (getPid, shell, createProcess)
import qualified System.Process.Typed as P


data MonitoredProcess = MonitoredProcess
  { name        :: T.Text
  , process     :: Process
  , memoryUsage :: Int
  , uptime      :: Int
  , status      :: T.Text
  , logFile     :: FilePath} deriving (Generic, Show, ToJSON, FromJSON)


processState :: Process -> IO String
processState p = do
  alive <- isAlive p
  return $ if alive then "Started" else "Stopped"


-- start a process as a monitored process given a unique name and a
-- command to start it
monitoredProcess :: T.Text -> T.Text -> IO (Maybe MonitoredProcess)
monitoredProcess name cmd = do
  startedProcess <- start name cmd
  loggingDirectory <- Internal.loggingDirectory
  case startedProcess of
    Just p -> do
      state <- processState p
      return $ Just MonitoredProcess { name        = name
                                , process     = p
                                , memoryUsage = 0
                                , uptime      = 0
                                , status      = T.pack state
                                , logFile     = loggingDirectory </> T.unpack name
                                }

    Nothing -> do
      return Nothing


startM :: T.Text -> T.Text -> IO (Maybe MonitoredProcess)
startM name cmd = do
  unique <- isUnique name
  M.guard $ unique
  mprocess <- monitoredProcess name cmd
  case mprocess of
    Just p -> do
      add p
      return $ Just p
    Nothing -> do
      return Nothing
  where
    isUnique :: T.Text -> IO Bool
    isUnique processName = do
      processes <- listAll
      case processes of
        Just procs -> do
          putStrLn $ show (processesNames procs)
          return $ not $ processName `elem` (processesNames procs)
        Nothing -> return True


processesNames :: [MonitoredProcess] -> [T.Text]
processesNames procs = map (\p -> name p) procs


listAll :: IO (Maybe [MonitoredProcess])
listAll = do
  confFile <- Internal.configFile
  processes <- B.readFile confFile
  return $ decode processes


add :: MonitoredProcess -> IO ()
add process = do
  alreadyExistingProcesses <- listAll
  case alreadyExistingProcesses of
    Just aprocesses ->
      save $ process:aprocesses
    Nothing ->
      save $ process:[]


save :: [MonitoredProcess] -> IO ()
save processes = do
  confFile <- Internal.configFile
  B.writeFile confFile (encode $ processes)
