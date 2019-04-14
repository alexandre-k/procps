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
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Char
import Data.List
import qualified Data.String as S
import Data.String.Utils
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import qualified System.Process.Typed as P


data FilterProperty = PName | Command

data Process = Process
  { pname :: String
  , pid :: FilePath
  , command :: String
  } deriving Show


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
  name <- readFile processName
  cmd <- readFile processCommand
  return $ Process { pid = p, pname = strip name, command = cmd }
  where
    processName = Internal.processesDir </> p </> Internal.processName
    processCommand = Internal.processesDir </> p </> Internal.processCommand

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
