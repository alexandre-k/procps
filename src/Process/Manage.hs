{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Process.Manage
  ( FilterProperty (..)
  , createMonitoredProcess
  , stopMonitoredProcess
  , listProcesses
  , isRunning
  , findProcess
  , kill
  , start
  )
where

import qualified Process.Internal.Common as Internal
import Control.Monad as M
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.String.Utils
import GHC.Generics
import System.Directory
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

-- list currently running processes as process IDs
runningProcesses :: IO [FilePath]
runningProcesses = do
  directories <- listDirectory Internal.processesDir
  return $ filter Internal.isInteger directories

-- list currently running processes as the Process data type
listProcesses :: IO [Process]
listProcesses = do
  procs <- runningProcesses
  mapM readProcessInfo procs

-- check if a process is being currently run
isRunning :: String -> IO Bool
isRunning name = do
  processes <- listProcesses
  return $ empty $ filterProcesses processes PName name
  where
    empty = not . null

-- filter a process(es) by its name or a keyword used for its execution
filterProcesses :: [Process] -> FilterProperty -> String -> [Process]
filterProcesses processes filterProperty keyword =
  filter (hasKeyword . property) processes
    where
      hasKeyword = (isInfixOf keyword)
      property = case filterProperty of
        PName -> pname
        Command -> command

-- find one or several process by its name or a keyword used at the
-- command line to execute it.
findProcess :: FilterProperty -> String -> IO [Process]
findProcess filterProperty keyword = do
  processes <- listProcesses
  return $ filterProcesses processes filterProperty keyword

-- read information of interest for a given process found in /proc
readProcessInfo :: FilePath -> IO Process
readProcessInfo p = do
  name <- readFile (Internal.processName p)
  cmd <- readFile (Internal.processCommand p)
  return $ Process { pid = p, pname = strip name, command = cmd }

-- kill a process given a Process data type
kill :: Process -> IO (ExitCode)
kill process =
  let cmd = "kill -9 " ++ (pid process)
  in do
    exitCode <- P.runProcess $ P.shell cmd
    return exitCode

-- start a process given a command
start :: String -> IO ()
start cmd = P.runProcess_ $ P.shell cmd

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


createMonitoredProcess :: String -> String -> IO ()
createMonitoredProcess name cmd = do
  unique <- isUnique name
  M.guard $ unique
  process <- monitoredProcess name cmd
  addMonitoredProcess process
  where

    isUnique :: String -> IO Bool
    isUnique processName = do
      confFile <- Internal.configFile
      processes <- B.readFile confFile
      case decode processes of
        Just dprocesses -> return $ not $ processName `elem` (map pname dprocesses)
        Nothing -> return True


stopMonitoredProcess :: String -> IO ()
stopMonitoredProcess name = do
  putStrLn "stop monitored process"
  processes <- listMonitoredProcesses
  case processes of
    Just p -> putStrLn $ show p
    Nothing -> putStrLn "nothing"
  -- where
  --   removeProcess :: String -> MonitoredProcess
  --   removeProcess name = do
  --     putStrLn "remove Process"


listMonitoredProcesses :: IO (Maybe [MonitoredProcess])
listMonitoredProcesses = do
  confFile <- Internal.configFile
  processes <- B.readFile confFile
  return $ decode processes


addMonitoredProcess :: MonitoredProcess -> IO ()
addMonitoredProcess process = do
  alreadyExistingProcesses <- listMonitoredProcesses
  confFile <- Internal.configFile
  case alreadyExistingProcesses of
    Just aprocesses ->
      B.writeFile confFile (encode $ process:aprocesses)
    Nothing ->
      B.writeFile confFile (encode process)
