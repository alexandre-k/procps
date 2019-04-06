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
  } deriving Show


linuxProcessesDir :: String
linuxProcessesDir = "/proc"

linuxProcessName :: String
linuxProcessName = "comm"

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

findProcess :: String -> IO [Process]
findProcess name = do
  procs <- listProcesses
  return $ findProcessByName name procs

readName :: FilePath -> IO Process
readName p = do
  name <- readFile process
  return $ Process {pid = p, pname = strip name}
  where
    process = linuxProcessesDir </> p </> linuxProcessName

kill :: Process -> ExitCode
kill process =
  let cmd = "kill -9 " ++ (pid process)
  in
    do
        exit <- P.runProcess $ P.shell cmd
        exit

main = do
  kateProcs <- findProcess "kate"
  result <- kill $ kate
  putStrLn $ show result
  where
    kate = kateProcs !! 0
  -- p <- processes
  -- print p
