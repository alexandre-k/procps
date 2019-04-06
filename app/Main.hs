module Main where

import Data.Char
import Data.List
import Data.String.Utils
import System.Directory
import System.FilePath.Posix
import System.IO


data Process = Process
  { pname :: String
  , pid :: FilePath
  }


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

listProcessNames :: IO [Process]
listProcessNames = do
  procs <- processes
  mapM readName procs

isRunningProcess :: String -> IO Bool
isRunningProcess name = do
  procs <- listProcessNames
  return $ isFoundProcess name procs

isFoundProcess :: String -> [Process] -> Bool
isFoundProcess name procs =
  any (hasName . pname) procs
  where
    hasName = (isInfixOf name)


-- findProcess :: String -> (Int, String)
-- findProcess name = do
--   procs <- listProcessNames
--   return

readName :: FilePath -> IO Process
readName p = do
  name <- readFile process
  return $ Process {pid = p, pname = strip name}
  where
    process = linuxProcessesDir </> p </> linuxProcessName

main = do
  result <- isRunningProcess "firefox"
  putStrLn $ show result
  -- p <- processes
  -- print p
