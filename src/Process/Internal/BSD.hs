module Process.Internal.BSD where

import Data.Char


processesDir :: String
processesDir = "/proc"

processName :: String
processName = "comm"

processCommand :: String
processCommand = "cmdline"

processEnviron :: String
processEnviron = "environ"

processCwd :: String
processCwd = "cwd"

cpuInfo :: String
cpuInfo = "cpuinfo"

cpuUsage :: String
cpuUsage = "stat"

loadAvg :: String
loadAvg = "loadavg"

-- helper function to filter processes in /proc
isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs
