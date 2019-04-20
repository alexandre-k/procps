{-# LANGUAGE CPP #-}
module Process.Internal.Common where

import Data.Char
#if linux_HOST_OS
import qualified Process.Internal.Linux as Linux


processesDir :: String
processesDir = Linux.processesDir

processName :: String
processName = Linux.processName

processCommand :: String
processCommand = Linux.processName

processEnviron :: String
processEnviron = Linux.processEnviron

processCwd :: String
processCwd = Linux.processCwd

cpuInfo :: String
cpuInfo = Linux.cpuInfo

cpuUsage :: String
cpuUsage = Linux.cpuInfo

loadAvg :: String
loadAvg = Linux.loadAvg
#else
import qualified Process.Internal.BSD as BSD


processesDir :: String
processesDir = BSD.processesDir

processName :: String
processName = BSD.processName

processCommand :: String
processCommand = BSD.processName

processEnviron :: String
processEnviron = BSD.processEnviron

processCwd :: String
processCwd = BSD.processCwd

cpuInfo :: String
cpuInfo = BSD.cpuInfo

cpuUsage :: String
cpuUsage = BSD.cpuUsage

loadAvg :: String
loadAvg = BSD.loadAvg
#endif

-- helper function to filter processes in /proc
isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs
