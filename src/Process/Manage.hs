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
  , stop
  )
where

import qualified Process.Internal.Common as Internal
import Data.Aeson
import qualified Data.List as L
import qualified Data.Text as T
import GHC.Generics
import System.Directory
import System.Exit
import qualified System.Process.Typed as PT
import qualified System.Process as P

data FilterProperty = PName | Command

data Process = Process
  { pname   :: T.Text
  , pid     :: FilePath
  , command :: T.Text
  } deriving (Generic, Show, ToJSON, FromJSON)


-- list currently running processes as process IDs
runningProcesses :: IO [FilePath]
runningProcesses = do
  directories <- listDirectory Internal.processesDir
  return $ L.filter Internal.isInteger directories

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
isRunning :: T.Text -> IO Bool
isRunning name = do
  processes <- listProcesses
  return $ empty $ filterProcesses processes PName name
  where
    empty = not . null

-- filter a process(es) by its name or a keyword used for its execution
filterProcesses :: [Process] -> FilterProperty -> T.Text -> [Process]
filterProcesses processes filterProperty keyword =
  filter (hasKeyword . property) processes
    where
      hasKeyword = (T.isInfixOf keyword)
      property = case filterProperty of
        PName -> pname
        Command -> command

-- find one or several process by its name or a keyword used at the
-- command line to execute it.
findProcess :: FilterProperty -> T.Text -> IO [Process]
findProcess filterProperty keyword = do
  processes <- listProcesses

  let processes' = filter (\p -> not $ isBlank (command p)) processes
    in
    return $ filterProcesses processes' filterProperty keyword
  where
    isBlank :: T.Text -> Bool
    isBlank s = T.null . T.strip $ s

-- read information of interest for a given process found in /proc
readProcessInfo :: FilePath -> IO Process
readProcessInfo p = do
  name <- readFile (Internal.processName $ T.pack p)
  cmd <- readFile (Internal.processCommand $ T.pack p)
  return $ Process { pid = p, pname = T.strip . T.pack $ name, command = T.strip . T.pack $ cmd }

-- kill a process given a Process data type
kill :: Process -> IO (ExitCode)
kill process =
  let cmd = "kill -9 " ++ (pid process)
  in do
    exitCode <- PT.runProcess $ PT.shell cmd
    return exitCode

-- start a process given a command
start :: T.Text -> T.Text -> IO (Maybe Process)
start name cmd = do
  hdl <- P.spawnCommand $ T.unpack cmd
  procPid <- P.getPid hdl
  case procPid of
    Just p -> return $ Just Process { pname   = name
                                    , pid     = show $ p
                                    , command = cmd }
    Nothing -> return Nothing


stop :: Process -> IO ExitCode
stop process = do
    exists <- isAlive process
    if exists then kill process else return $ ExitFailure 127
