module ProcPS
  ( FilterProperty (..)
  , listProcesses
  , isRunning
  , findProcess
  , kill
  )
where

import Data.Char
import Data.List
import Data.String.Utils
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import qualified System.Process.Typed as P


data FilterProperty = PName | PID | Command

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

isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs

runningProcesses :: IO [FilePath]
runningProcesses = do
  directories <- listDirectory linuxProcessesDir
  return $ filter isInteger directories

listProcesses :: IO [Process]
listProcesses = do
  procs <- runningProcesses
  mapM readName procs

isRunning :: String -> IO Bool
isRunning name = do
  processes <- listProcesses
  return $ empty $ filterProcesses processes PName name
  where
    empty = not . null

filterProcesses :: [Process] -> FilterProperty -> String -> [Process]
filterProcesses processes filterProperty keyword =
  filter (hasKeyword . property) processes
    where
      hasKeyword = (isInfixOf keyword)
      property = case filterProperty of
        PName -> pname
        Command -> command

findProcess :: FilterProperty -> String -> IO [Process]
findProcess filterProperty keyword = do
  processes <- listProcesses
  return $ filterProcesses processes filterProperty keyword

readName :: FilePath -> IO Process
readName p = do
  name <- readFile processName
  cmd <- readFile processCommand
  return $ Process { pid = p, pname = strip name, command = cmd }
  where
    processName = linuxProcessesDir </> p </> linuxProcessName
    processCommand = linuxProcessesDir </> p </> linuxProcessCommand

kill :: Process -> IO (ExitCode)
kill process =
  let cmd = "kill -9 " ++ (pid process)
  in do
    exitCode <- P.runProcess $ P.shell cmd
    return exitCode
