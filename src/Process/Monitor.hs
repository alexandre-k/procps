{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Process.Monitor
  ( MonitoredProcess (..)
  , listAll
  , monitoredProcess
  , start
  , startM
  )
where

import qualified Process.Internal.Common as Internal
import Process.Manage (Process(..), isAlive, kill)
import Control.Monad as M
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import System.Exit
import System.FilePath.Posix
import System.Process (getPid, shell, createProcess)
import qualified System.Process.Typed as P


data MonitoredProcess = MonitoredProcess
  { name        :: String
  , process     :: Process
  , started     :: Bool
  , memoryUsage :: Int
  , uptime      :: Int
  , status      :: String
  , logFile     :: FilePath} deriving (Generic, Show, ToJSON, FromJSON)


-- start a process as a monitored process given a unique name and a
-- command to start it
monitoredProcess :: String -> String -> IO MonitoredProcess
monitoredProcess name cmd = do
  startedProcess <- start name cmd
  loggingDirectory <- Internal.loggingDirectory
  case startedProcess of
    Just process -> do
      putStrLn $ show monitoredProc
      putStrLn $ show process
      return $ monitoredProc
      where
        monitoredProc = MonitoredProcess { name        = name
                                         , process     = process
                                         , started     = True
                                         , memoryUsage = 0
                                         , uptime      = 0
                                         , status      = "Running"
                                         , logFile     = loggingDirectory </> name
                                         }

    Nothing -> do
      return $ MonitoredProcess { name = name
                                , process = Process { pname = name
                                                    , pid = "-1"
                                                    , command = cmd
                                                    }
                                , started = False
                                , memoryUsage = 0
                                , uptime = 0
                                , status = "Stopped"
                                , logFile = loggingDirectory </> name
                                }


startM :: String -> String -> IO ()
startM name cmd = do
  unique <- isUnique name
  M.guard $ unique
  process <- monitoredProcess name cmd
  add process
  where
    isUnique :: String -> IO Bool
    isUnique processName = do
      processes <- listAll
      case processes of
        Just procs -> return $ processName `elem` (processesNames procs)
        Nothing -> return True


processesNames :: [MonitoredProcess] -> [String]
processesNames procs = map (\p -> name p) procs


start :: String -> String -> IO (Maybe Process)
start name cmd = do
  (_, _, _, hdl) <- createProcess $ shell cmd
  pid <- getPid hdl
  case pid of
    Just p-> return $ Just Process { pid     = (show p) :: FilePath
                                    , pname   = name
                                    , command = cmd
                                    }
    Nothing -> return $ Nothing


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
