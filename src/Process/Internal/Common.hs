{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Process.Internal.Common where

import Data.Char
import Data.Text
import System.Directory
import System.FilePath.Posix
#if linux_HOST_OS
import qualified Process.Internal.Linux as Linux


processesDir :: FilePath
processesDir = Linux.processesDir

processName :: Text -> FilePath
processName pid = Linux.processesDir </> (unpack pid) </> Linux.processName

processCommand :: Text -> FilePath
processCommand pid = Linux.processesDir </> (unpack pid) </> Linux.processCommand

processEnviron :: Text -> FilePath
processEnviron pid = Linux.processesDir </> (unpack pid) </> Linux.processEnviron

processCwd :: Text -> FilePath
processCwd pid = Linux.processesDir </> (unpack pid) </> Linux.processCwd

cpuInfo :: FilePath
cpuInfo = Linux.cpuInfo </> Linux.cpuInfo

cpuUsage :: FilePath
cpuUsage = Linux.processesDir </> Linux.cpuUsage

loadAvg :: FilePath
loadAvg = Linux.processesDir </> Linux.loadAvg
#else
import qualified Process.Internal.BSD as BSD


processesDir :: Text
processesDir = BSD.processesDir

processName :: Text -> FilePath
processName pid = BSD.processesDir </> (unpack pid) </> BSD.processName

processCommand :: Text -> FilePath
processCommand pid = BSD.processesDir </> (unpack pid) </> BSD.processCommand

processEnviron :: Text -> FilePath
processEnviron pid = BSD.processesDir </> (unpack pid) </> BSD.processEnviron

processCwd :: Text -> FilePath
processCwd pid = BSD.processesDir </> (unpack pid) </> BSD.processCwd

cpuInfo :: FilePath
cpuInfo = BSD.cpuInfo </> BSD.cpuInfo

cpuUsage :: FilePath
cpuUsage = BSD.processesDir </> BSD.cpuUsage

loadAvg :: FilePath
loadAvg = BSD.processesDir </> BSD.loadAvg

#endif

-- helper function to filter processes in /proc
isInteger :: FilePath -> Bool
isInteger xs = Prelude.all isDigit xs


configDirectory :: IO FilePath
configDirectory = do
  home <- getHomeDirectory
  -- createDirectory $ home </> ".procps"
  return $ home </> ".procps"

loggingDirectory :: IO FilePath
loggingDirectory = do
  conf <- configDirectory
  return $ conf </> "logs"

configFile :: IO FilePath
configFile = do
  confDir <- configDirectory
  return $ confDir </> "processes.json"
