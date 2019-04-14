module Process.Internal.Linux where

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

-- helper function to filter processes in /proc
isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs
