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

isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs

runningProcesses :: IO [FilePath]
runningProcesses = do
  directories <- listDirectory linuxProcessesDir
  return $ filter isInteger directories

listProcesses :: IO [Process]
listProcesses = do
  procs <- runningProcesses
  mapM readProcessInfo procs

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

readProcessInfo :: FilePath -> IO Process
readProcessInfo p = do
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

start :: String -> IO ()
start cmd = P.runProcess_ $ P.shell cmd

formatEnviron :: String -> [(String, String)]
formatEnviron environ = map (splitAtFirst "=") ((split "\0") environ)

splitAtFirst :: String -> String -> (String, String)
splitAtFirst sep [] = ("", "")
splitAtFirst sep str =
  (key, value)
  where
    keyValues = split sep str
    key = head keyValues
    value = join "=" (tail keyValues)

processEnviron :: String -> IO [(String, String)]
processEnviron process = do
  environ <- readFile procEnviron
  return $ formatEnviron environ
  where
    procEnviron = linuxProcessesDir </> process </> linuxProcessEnviron

seeCwd :: String -> IO String
seeCwd process = do
  (out, _) <- P.readProcess_ $ P.shell cmd
  return $ strip $ C8.unpack $ out
  where
    cmd = "readlink " ++ linuxProcessesDir </> process </> linuxProcessCwd
