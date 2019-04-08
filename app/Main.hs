module Main
  ( listProcesses
  , isRunning
  , findProcess
  , kill
  , main
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

processes :: IO [FilePath]
processes = do
  directories <- listDirectory linuxProcessesDir
  return $ filter isInteger directories

listProcesses :: IO [Process]
listProcesses = do
  procs <- processes
  mapM readName procs

isRunning :: String -> IO Bool
isRunning name = do
  procs <- listProcesses
  return $ empty $ findProcessByName name procs
  where
    empty = not . null

findProcessByName :: String -> [Process] -> [Process]
findProcessByName name procs =
  filter (hasName . pname) procs
  where
    hasName = (isInfixOf name)

findProcess :: String -> String -> IO [Process]
findProcess name keyword = do
  procs <- listProcesses
  return $ findProcessByName name procs

readName :: FilePath -> IO Process
readName p = do
  name <- readFile processName
  cmd <- readFile processCommand
  return $ Process {pid = p, pname = strip name, command = cmd }
  where
    processName = linuxProcessesDir </> p </> linuxProcessName
    processCommand = linuxProcessesDir </> p </> linuxProcessCommand

kill :: Process -> IO (ExitCode)
kill process =
  let cmd = "kill -9 " ++ (pid process)
  in
    do
      exitCode <- P.runProcess $ P.shell cmd
      return exitCode

main = do
  kate <- findProcess "kate" ""
  result <- kill $ kate !! 0
  putStrLn $ show result
  -- p <- processes
  -- print p
