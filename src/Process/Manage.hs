{-# LANGUAGE OverloadedStrings #-}

module Process.Manage
  ( FilterProperty (..)
  , listProcesses
  , isRunning
  , findProcess
  , kill
  , start
  )
where

import qualified Process.Internal.Common as Internal
import Data.List
import Data.String.Utils
import System.Directory
import System.Exit
import qualified System.Process.Typed as P


data FilterProperty = PName | Command


data Process = Process
  { pname   :: String
  , pid     :: FilePath
  , command :: String
  } deriving Show


data MonitoredProcesses = MonitoredProcess
  { processs    :: Process
  , started     :: Bool
  , stopped     :: Bool
  , memoryUsage :: Int
  , uptime      :: Int
  , status      :: String
  , logFile     :: FilePath}



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


monitor :: String -> String -> (Maybe Process)
monitor name cmd = do
  exitCode <- P.runProcess $ P.shell cmdWithPID
  case exitCode of
    ExitSuccess -> Just Process { pid = (show exitCode) :: FilePath
                                , pname = name
                                , command = cmd
                                }
    ExitFailure code -> Nothing
  where
    cmdWithPID = "(" ++ cmd ++ " &) && (echo $!)"
