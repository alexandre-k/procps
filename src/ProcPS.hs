{-# LANGUAGE OverloadedStrings #-}
module ProcPS
  ( FilterProperty (..)
  , listProcesses
  , isRunning
  , findProcess
  , kill
  , processEnviron
  , start
  , seeCwd
  )
where

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


linuxProcessesDir :: String
linuxProcessesDir = "/proc"

linuxProcessName :: String
linuxProcessName = "comm"

linuxProcessCommand :: String
linuxProcessCommand = "cmdline"

linuxProcessEnviron :: String
linuxProcessEnviron = "environ"

linuxProcessCwd :: String
linuxProcessCwd = "cwd"

-- helper function to filter processes in /proc
isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs

-- list currently running processes as process IDs
runningProcesses :: IO [FilePath]
runningProcesses = do
  directories <- listDirectory linuxProcessesDir
  return $ filter isInteger directories

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
    processName = linuxProcessesDir </> p </> linuxProcessName
    processCommand = linuxProcessesDir </> p </> linuxProcessCommand

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

-- list all environment variables used to run a process
formatEnviron :: String -> [(String, String)]
formatEnviron environ = map (splitAtFirst "=") ((split "\0") environ)

-- helper function to show environment variables. Split keys and values at "="
splitAtFirst :: String -> String -> (String, String)
splitAtFirst sep [] = ("", "")
splitAtFirst sep str =
  (key, value)
  where
    keyValues = split sep str
    key = head keyValues
    value = join "=" (tail keyValues)

-- get environment variables used for a given process
processEnviron :: String -> IO [(String, String)]
processEnviron process = do
  environ <- readFile procEnviron
  return $ formatEnviron environ
  where
    procEnviron = linuxProcessesDir </> process </> linuxProcessEnviron

-- show the current working directory of a given process
seeCwd :: String -> IO String
seeCwd process = do
  (out, _) <- P.readProcess_ $ P.shell cmd
  return $ strip $ C8.unpack $ out
  where
    cmd = "readlink " ++ linuxProcessesDir </> process </> linuxProcessCwd
