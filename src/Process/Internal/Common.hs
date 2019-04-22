{-# LANGUAGE CPP #-}
module Process.Internal.Common where

import Data.Char
import System.Directory
import System.FilePath.Posix
#if linux_HOST_OS
import qualified Process.Internal.Linux as Linux


processesDir :: String
processesDir = Linux.processesDir

processName :: String -> FilePath
processName pid = Linux.processesDir </> pid </> Linux.processName

processCommand :: String -> FilePath
processCommand pid = Linux.processesDir </> pid </> Linux.processCommand

processEnviron :: String -> FilePath
processEnviron pid = Linux.processesDir </> pid </> Linux.processEnviron

processCwd :: String -> FilePath
processCwd pid = Linux.processesDir </> pid </> Linux.processCwd

cpuInfo :: FilePath
cpuInfo = Linux.cpuInfo </> Linux.cpuInfo

cpuUsage :: FilePath
cpuUsage = Linux.processesDir </> Linux.cpuUsage

loadAvg :: FilePath
loadAvg = Linux.processesDir </> Linux.loadAvg
#else
import qualified Process.Internal.BSD as BSD


processesDir :: String
processesDir = BSD.processesDir

processName :: String -> FilePath
processName pid = BSD.processesDir </> pid </> BSD.processName

processCommand :: String -> FilePath
processCommand pid = BSD.processesDir </> pid </> BSD.processCommand

processEnviron :: String -> FilePath
processEnviron pid = BSD.processesDir </> pid </> BSD.processEnviron

processCwd :: String -> FilePath
processCwd pid = BSD.processesDir </> pid </> BSD.processCwd

cpuInfo :: FilePath
cpuInfo = BSD.cpuInfo </> BSD.cpuInfo

cpuUsage :: FilePath
cpuUsage = BSD.processesDir </> BSD.cpuUsage

loadAvg :: FilePath
loadAvg = BSD.processesDir </> BSD.loadAvg

#endif

-- helper function to filter processes in /proc
isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs

configDirectory :: IO FilePath
configDirectory = do
  home <- getHomeDirectory
  return $ home </> ".procps"

loggingDirectory :: IO FilePath
loggingDirectory = do
  conf <- configDirectory
  return $ conf </> "logs"
