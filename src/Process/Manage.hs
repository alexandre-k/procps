{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Process.Manage
  ( FilterProperty (..)
  , Process (..)
  , listProcesses
  , isAlive
  , isRunning
  , findProcess
  , kill
  , start
  )
where

import qualified Process.Internal.Common as Internal
import Data.Aeson
import Data.List
import Data.String.Utils
import GHC.Generics
import System.Directory
import System.Exit
import qualified System.Process.Typed as P

data FilterProperty = PName | Command

data Process = Process
  { pname   :: String
  , pid     :: FilePath
  , command :: String
  } deriving (Generic, Show, ToJSON, FromJSON)


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

-- Determine if a process is alive based on all processes found
isAlive :: Process -> IO Bool
isAlive process = do
  processes <- listProcesses
  return $ not . null $ filter (isSameProcess process) processes
  where
    isSameProcess :: Process -> Process -> Bool
    isSameProcess p1 p2  =
      (pid p1) == (pid p2) && (pname p1) == (pname p2) && (command p1) == (command p2)

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
