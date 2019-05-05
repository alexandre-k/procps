{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Process.Monitor
  ( FilterProperty (..)
  , MonitoredProcess (..)
  , Process (..)
  , create
  , listAll
  , stop
  )
where

import qualified Process.Internal.Common as Internal
import Control.Monad as M
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import System.Exit
import System.FilePath.Posix
import qualified System.Process.Typed as P

data FilterProperty = PName | Command

data Process = Process
  { pname   :: String
  , pid     :: FilePath
  , command :: String
  } deriving (Generic, Show, ToJSON, FromJSON)

data MonitoredProcess = MonitoredProcess
  { name        :: String
  , process     :: Process
  , started     :: Bool
  , stopped     :: Bool
  , memoryUsage :: Int
  , uptime      :: Int
  , status      :: String
  , logFile     :: FilePath} deriving (Generic, Show, ToJSON, FromJSON)

-- start a process as a monitored process given a unique name and a
-- command to start it
monitoredProcess :: String -> String -> IO MonitoredProcess
monitoredProcess name cmd = do
  startedProcess <- startProcess name cmd
  loggingDirectory <- Internal.loggingDirectory
  case (startedProcess) of
    Just process -> do
      return $ MonitoredProcess { name        = name
                                , process     = process
                                , started     = True
                                , stopped     = False
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
                                , stopped = True
                                , memoryUsage = 0
                                , uptime = 0
                                , status = "Stopped"
                                , logFile = loggingDirectory </> name
                                }

  where

    startProcess :: String -> String -> IO (Maybe Process)
    startProcess name cmd = do
      exitCode <- P.runProcess $ P.shell cmdWithPID
      case exitCode of
          ExitSuccess -> return $ Just Process { pid = (show exitCode) :: FilePath
                                      , pname = name
                                      , command = cmd
                                      }
          ExitFailure _ -> return $ Nothing
      where
          cmdWithPID = "(" ++ cmd ++ " &) && (echo $!)"


create :: String -> String -> IO ()
create name cmd = do
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


stop :: String -> IO ()
stop name = do
  processes <- listAll
  case processes of
    Just p -> putStrLn $ show p
    Nothing -> putStrLn "nothing"
  -- where
  --   removeProcess :: String -> MonitoredProcess
  --   removeProcess name = do
  --     putStrLn "remove Process"


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
